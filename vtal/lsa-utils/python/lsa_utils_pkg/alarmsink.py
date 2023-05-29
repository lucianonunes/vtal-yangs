from enum import Enum
from datetime import datetime
from typing import List

from ncs import maagic, maapi
from ncs import OPERATIONAL

'''
This code is inspired by and closely follows the implementation provided
at https://gitlab.com/nso-developer/py-alarm-sink
'''


class Severity(Enum):
    ''' Defines severity-t data type
        typedef severity-t  {
            type enumeration {
            enum cleared {
                value 1;
            }
            enum indeterminate {
                value 2;
            }
            enum minor {
                value 3;
            }
            enum warning {
                value 4;
            }
            enum major {
                value 5;
            }
            enum critical {
                value 6;
            }
            }
            description
            "Severity of an alarm, including the cleared state";
        }
    '''
    CLEARED = 1
    INDETERMINATE = 2
    MINOR = 3
    WARNING = 4
    MAJOR = 5
    CRITICAL = 6


class Alarm():
    '''Local Mode Alarm Sink to submit nso process related alarms
    '''
    def __init__(self, type: str, managed_object: str, specific_problem: str = None):
        self.device = 'ncs'
        self.type = type
        self.managed_object = managed_object
        self.specific_problem = specific_problem
        self.timestamp = datetime.now().isoformat()
        self.impacted_objects = []

    def set_severity(self, severity: Severity):
        self.severity = severity
        return self

    def set_alarm_text(self, alarm_text: str):
        max_length = 1024
        if len(alarm_text) > max_length:
            truncated = ' ... [truncated]'
            self.alarm_text = alarm_text[:max_length - len(truncated)] + truncated
        else:
            self.alarm_text = alarm_text
        return self

    def set_impacted_objects(self, imp_objs: List[str]):
        self.impacted_objects = imp_objs
        return self

    def flush(self) -> None:
        '''Creates an alarm list entry.
        '''

        with maapi.single_write_trans("", "system", db=OPERATIONAL) as th:
            root = maagic.get_root(th)
            alarm_list = root.al__alarms.alarm_list.alarm

            al_key = (self.device, self.type, self.managed_object,
                      self.specific_problem or '')

            # If the alarm is cleared and CDB doesn't have it
            if self.cleared and not alarm_list.exists(al_key):
                return

            alarm = alarm_list.create(al_key)

            # Check if alarm already exists
            if alarm.is_cleared == self.cleared \
               and (self.cleared or
                    (alarm.last_perceived_severity.value == self.severity.value and
                     alarm.last_alarm_text == self.alarm_text)):
                return

            alarm.is_cleared = self.cleared
            alarm.last_alarm_text = self.alarm_text
            if not self.cleared:
                alarm.last_perceived_severity = self.severity.value
            alarm.last_status_change = self.timestamp

            for iobj in self.impacted_objects:
                alarm.impacted_objects.create(iobj)

            sc = alarm.status_change.create(self.timestamp)
            sc.perceived_severity = self.severity.value
            sc.alarm_text = self.alarm_text

            th.apply()

    @property
    def cleared(self):
        return self.severity == Severity.CLEARED

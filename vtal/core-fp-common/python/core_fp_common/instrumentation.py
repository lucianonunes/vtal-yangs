# -*- mode: python; python-indent: 4 -*-
import functools
import logging
from .common_utils import get_service_operation


ENTRY = "ENTRY_POINT:"
EXIT = "EXIT_POINT:"


def instrument(log_level, *dargs):
    """
    Decorator to instrument a generic Function. Takes two arguments:
        log_level: the logging.Level to log the data
        *dargs: define the arguments of the instrumented function to log, in
               the format of "arg_name:arg_index"
    For example:  @instrument(logging.DEBUG, "op:2", "kp:3")
    """
    def instrument_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                message = build_log_msg(ENTRY, [func.__name__], dargs, args)
                log(message)

            result = func(*args, **kwargs)
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                message = build_log_msg(EXIT, [func.__name__], dargs, args)
                log(message)
            return result
        return wrapper_instrument
    return instrument_func


def instrument_enter(log_level, *dargs):
    """
    Decorator to instrument the entry of a generic Function. Takes two
    arguments:
        log_level: the logging.Level to log the data
        *dargs: define the arguments of the instrumented function to log, in
               the format of "arg_name:arg_index"
    """
    def instrument_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                message = build_log_msg(ENTRY, [func.__name__], dargs, args)
                log(message)
            return func(*args, **kwargs)
        return wrapper_instrument
    return instrument_func


def instrument_exit(log_level, *dargs):
    """
    Decorator to instrument the exit of a generic Function. Takes two
    arguments:
        log_level: the logging.Level to log the data
        *dargs: define the arguments of the instrumented function to log, in
               the format of "arg_name:arg_index"
    """
    def instrument_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            result = func(*args, **kwargs)
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                message = build_log_msg(EXIT, [func.__name__], dargs, args)
                log(message)
            return result
        return wrapper_instrument
    return instrument_func


def instrument_validate(log_level, *dargs):
    """
    Instrument a function of validate_service. This decorator
    expects the following function parameters:

        (self, validation_callpoint, tctx, kp)
    """
    def instrument_validate_func(func):
        @functools.wraps(func)
        def wrapper_instrument_validate(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{ENTRY} validate_cb: {args[1]}, kp: {args[3]}")
            result = func(*args, **kwargs)
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log("{} validate_cb: {}, kp: {}".
                    format(EXIT, args[1], args[3]))
            return result
        return wrapper_instrument_validate
    return instrument_validate_func


def instrument_validate_enter(log_level, *dargs):
    """
    Instrument the entry of a function of validate_service. This
    decorator expects the following function parameters:
         (self, validation_callpoint, tctx, kp)
    """
    def instrument_validate_func(func):
        @functools.wraps(func)
        def wrapper_instrument_validate(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{ENTRY} validate_cb: {args[1]}, kp: {args[3]}")
            return func(*args, **kwargs)
        return wrapper_instrument_validate
    return instrument_validate_func


def instrument_validate_exit(log_level, *dargs):
    """
    Instrument the exit of a function of validate_service.  This
    decorator expects the following function parameters:
        (self, validation_callpoint, tctx, kp)
    """
    def instrument_service_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{EXIT} validate_cb: {args[1]}, kp: {args[3]}")
            return func(*args, **kwargs)
        return wrapper_instrument
    return instrument_service_func


def instrument_service(log_level, servicepoint, *dargs):
    """
    Instrument a function of ncs.application.Service. This decorator
    expects the following function parameters:
        (self, tctx, op, kp, root, proplist)
    """
    def instrument_service_func(func):
        @functools.wraps(func)
        def wrapper_instrument_service(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{ENTRY} "
                    f"servicepoint: {servicepoint}, "
                    f"cb: {func.__name__}, "
                    f"kp: {args[3]}, "
                    f"service op: {get_service_operation(args[2])}")
            result = func(*args, **kwargs)
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{EXIT} "
                    f"servicepoint: {servicepoint}, "
                    f"cb: {func.__name__}, "
                    f"kp: {args[3]}, "
                    f"service op: {get_service_operation(args[2])}")
            return result
        return wrapper_instrument_service
    return instrument_service_func


def instrument_service_enter(log_level, servicepoint, *dargs):
    """
    Instrument the entry of a function of ncs.application.Service. This
    decorator expects the following function parameters:
        (self, tctx, op, kp, root, proplist)
    """
    def instrument_service_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{ENTRY} "
                    f"servicepoint: {servicepoint}, "
                    f"cb: {func.__name__}, "
                    f"kp: {args[3]}, "
                    f"service op: {get_service_operation(args[2])}")
            return func(*args, **kwargs)
        return wrapper_instrument
    return instrument_service_func


def instrument_service_exit(log_level, servicepoint, *dargs):
    """
    Instrument the exit of a function of ncs.application.Service.  This
    decorator expects the following function parameters:
        (self, tctx, op, kp, root, proplist)
    """
    def instrument_service_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{EXIT} "
                    f"servicepoint: {servicepoint}, "
                    f"cb: {func.__name__}, "
                    f"kp: {args[3]}, "
                    f"service op: {get_service_operation(args[2])}")
            return func(*args, **kwargs)
        return wrapper_instrument
    return instrument_service_func


def instrument_nano(log_level, servicepoint, *dargs):
    """
    instrument a function of ncs.application.NanoService. This decorator
    expects the following function parameters:
        (self, tctx, root, service, plan, component, state, opaque, comp_vars)
    """
    def instrument_nano_func(func):
        @functools.wraps(func)
        def wrapper_instrument_nano(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                service = get_service_id(args[3]._path)
                (component_type, component_name) = args[5]
                state = args[6].split(":")[1]
                log(f"{ENTRY}, "
                    f"servicepoint: {servicepoint}, "
                    f"service: {service}, "
                    f"component: {component_type} {component_name}, "
                    f"state: {state}, "
                    f"nano op: {func.__name__}")
            result = func(*args, **kwargs)
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                service = get_service_id(args[3]._path)
                (component_type, component_name) = args[5]
                state = args[6].split(":")[1]
                log(f"{EXIT}, "
                    f"servicepoint: {servicepoint}, "
                    f"service: {service}, "
                    f"component: {component_type} {component_name}, "
                    f"state: {state}, "
                    f"nano op: {func.__name__}")
            return result
        return wrapper_instrument_nano
    return instrument_nano_func


def instrument_nano_enter(log_level, servicepoint, *dargs):
    """
    Instrument the entry of a function of ncs.application.NanoService.  This
    decorator expects the following function parameters:
        (self, tctx, root, service, plan, component, state, opaque, comp_vars)
    """
    def instrument_nano_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                service = get_service_id(args[3]._path)
                (component_type, component_name) = args[5]
                state = args[6].split(":")[1]
                log(f"{ENTRY}, "
                    f"servicepoint: {servicepoint}, "
                    f"service: {service}, "
                    f"component: {component_type} {component_name}, "
                    f"state: {state}, "
                    f"nano op: {func.__name__}")
            return func(*args, **kwargs)
        return wrapper_instrument
    return instrument_nano_func


def instrument_nano_exit(log_level, servicepoint, *dargs):
    """
    Instrument the exit of a function of ncs.application.NanoService.  This
    decorator expects the following function parameters:
        (self, tctx, root, service, plan, component, state, opaque, comp_vars)
    """
    def instrument_nano_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            result = func(*args, **kwargs)
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                service = get_service_id(args[3]._path)
                (component_type, component_name) = args[5]
                state = args[6].split(":")[1]
                log(f"{EXIT}, "
                    f"servicepoint: {servicepoint}, "
                    f"service: {service}, "
                    f"component: {component_type} {component_name}, "
                    f"state: {state}, "
                    f"nano op: {func.__name__}")
            return result
        return wrapper_instrument
    return instrument_nano_func


def instrument_action(log_level, *dargs):
    """
    Instrument an NSO Action handler.  This decorator expects the following
    function parameters:
        (self, uinfo, name, kp, input, output)
    """
    def instrument_action_func(func):
        @functools.wraps(func)
        def wrapper_instrument(*args, **kwargs):
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{ENTRY} name: {args[2]}, kp: {args[3]}")
            func(*args, **kwargs)
            if logging.getLogger().isEnabledFor(log_level):
                log = get_logger(args[0].log, log_level)
                log(f"{EXIT} name: {args[2]}, kp: {args[3]}")
        return wrapper_instrument
    return instrument_action_func


def get_logger(logger, level):
    # logging.FATAL and logging.CRITICAL are same
    # logging.CRITICAL = logging.FATAL = 50
    if level == logging.INFO:
        return logger.info
    elif level == logging.WARNING:
        return logger.warn
    elif level == logging.CRITICAL:
        return logger.critical
    elif level == logging.ERROR:
        return logger.error
    else:
        return logger.debug


def get_service_id(kp):
    kpath = str(kp)
    service = kpath[kpath.find("{") + 1: len(kpath) - 1]
    return service


def build_log_msg(prefix, vals, dargs, args):
    str = f"{prefix}:: function: {vals[0]}"
    for darg in dargs:
        p = darg.split(":")
        argv = f"{args[int(p[1])]}"
        str += f", {p[0]}: {argv} "
    return str

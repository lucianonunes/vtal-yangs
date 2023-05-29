class AsciiArt:
    """
    All ASCII ART for tools and visual debugging
    """

    booting = r"""
 ___            _    _
| _ ) ___  ___ | |_ (_) _ _   __ _
| _ \/ _ \/ _ \|  _|| || ' \ / _` | _  _  _
|___/\___/\___/ \__||_||_||_|\__, |(_)(_)(_)
                             |___/
"""
    up = r"""
 _   _  ___
| | | || _ \
| |_| ||  _/
 \___/ |_|  """
    goodbye = r"""
  ___               _  ___             _
 / __| ___  ___  __| || _ ) _  _  ___ | |
| (_ |/ _ \/ _ \/ _` || _ \| || |/ -_)|_|
 \___|\___/\___/\__,_||___/ \_, |\___|(_)
                            |__/
"""
    torpedo = r"~^~^~^~^~^~^~^~'=========>'~^~^~^~^~^~^~^~^~"  # For service create
    blackwidow = (
        r"------------------------------------------------"  # For service delete
    )
    caterpillar = (
        r"6oO0OoO0OoO0Oo 6oO0OoO0OoO0Oo 6oO0OoO0OoO0Oo 6oO0OoO0OoO0Oo"  # For redeploys
    )
    wave = r'_.~"~._.~"~._.~"~._.~"~._.~"~._.~"~._.~"~.__.~"~._.~"~._'  # For Updates
    roadblock = (
        r"X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X"  # For exceptions
    )
    # For end of service
    hibernate = r"ZZZZzzz zz z...ZZZZzzz zz z...ZZZZzzz zz z...ZZZZzzz zz z..."
    # For waiting [Notif/Poll]
    coffee_break = r"|_|>  |_|>  |_|>  |_|>  |_|>  |_|>  |_|>  |_|>  |_|>"
    # For Validate start/end
    castle = r":.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:"
    ninja_star = (
        r"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * "  # For Actions
    )


class StrConstants:
    cq_alarm_services = ["connectivity-service", "zr/dco"]
    transient_error_message = [
        "connection refused",
        "timed out",
        "host is unreachable",
        "no route to host",
        "host unreachable",
        "transport timeout",
        "ssh host key mismatch",
        "failed to connect to device",
    ]

# -*- mode: python; python-indent: 4 -*-

_ipv4_size = 32
_ipv4_max = 2 ** _ipv4_size - 1


def getIpAddress(addr):
    """Return the Ip part of a 'Ip/Net' string."""
    parts = addr.split("/")
    return parts[0]


def getIpPrefix(addr):
    """Return the Net part of a 'Ip/Net' string."""
    parts = addr.split("/")
    return parts[1]


def getNetMask(addr):
    """Get the NetMask from a 'Ip/Net' string."""
    return ipv4_int_to_str(prefix_to_netmask(int(getIpPrefix(addr))))


# def getNextIPV4Address(addr):
#     """Get the next succeeding IP address...hm..."""
#     i = ipv4_str_to_int(getIpAddress(addr)) + 1

#     if i > _ipv4_max:
#         raise ValueError("next IPV4 address out of bound")
#     else:
#         if (i & 0xFF) == 255:
#             i += 2

#     return ipv4_int_to_str(i)


# def prefixToWildcardMask(prefix):
#     """Transform a prefix (as string) to a netmask (as a string)."""
#     return ipv4_int_to_str(prefix_to_netmask(int(prefix)))


def prefix_to_netmask(prefix):
    """Transform an IP integer prefix to a netmask integer."""
    global _ipv4_size
    global _ipv4_max
    if (prefix >= 0) and (prefix <= _ipv4_size):
        return _ipv4_max ^ (2 ** (_ipv4_size - prefix) - 1)
    else:
        raise ValueError("IPV4 prefix out of bound")


# def ipv4_str_to_int(addr):
#     """Transform an IPV4 address string to an integer."""
#     parts = addr.split(".")
#     if len(parts) == 4:
#         return (
#             (int(parts[0]) << 24)
#             | (int(parts[1]) << 16)
#             | (int(parts[2]) << 8)
#             | int(parts[3])
#         )
#     else:
#         raise ValueError("wrong format of IPV4 string")


def ipv4_int_to_str(value):
    """Transform an IP integer to a string"""
    global _ipv4_max
    if (value >= 0) and (value <= _ipv4_max):
        return "%d.%d.%d.%d" % (
            value >> 24,
            (value >> 16) & 0xFF,
            (value >> 8) & 0xFF,
            value & 0xFF,
        )
    else:
        raise ValueError("IPV4 value out of bound")

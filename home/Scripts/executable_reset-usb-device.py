#!/usr/bin/env python3
#
# Reset the Native Instruments Komplete Audio USB device. This script looks up
# the device ID for the device and then resets it via ioctl(2). The script
# requires that the lsusb command is in PATH.

import collections
import fcntl
import re
import subprocess

Device = collections.namedtuple('Device', ['bus', 'device', 'rest'])

device_rx = re.compile(r"Bus (\d+) Device (\d+): (.*)")

def parse_devices(lsusb_output : str) -> list[Device]:
    res : list[Device] = []
    for line in lsusb_output.splitlines():
        md = re.match(device_rx, line)
        if md is None:
            continue

        res.append(Device(md.group(1), md.group(2), md.group(3)))

    return res

audio_rx = re.compile(r"Native Instruments Komplete Audio")

# The constant from the kernel
USBDEVFS_RESET = ord('U') << (4*2) | 20

def main():
    usb_output = subprocess.run(['lsusb'], stdout=subprocess.PIPE, text=True)
    devices = parse_devices(usb_output.stdout)

    for dev in devices:
        if re.search(audio_rx, dev.rest):
            print("Resetting USB device: {}".format(dev))
            dev_filename = '/dev/bus/usb/{}/{}'.format(dev.bus, dev.device)
            dev_file = open(dev_filename, 'w')
            _ = fcntl.ioctl(dev_file, USBDEVFS_RESET, 0)

if __name__ == '__main__':
    main()

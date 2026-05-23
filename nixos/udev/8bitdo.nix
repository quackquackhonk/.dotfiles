{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.udev.extraRules = ''
# bluetooth and 2.4Ghz dongle
KERNEL=="hidraw\*", ATTRS{idProduct}=="6012", ATTRS{idVendor}=="2dc8", MODE="0660", GROUP="input"
KERNEL=="hidraw\*", KERNELS=="\*2DC8:6012\*", MODE="0660", GROUP="input"

# passthrough for bottles
SUBSYSTEM=="usb", ATTR{idVendor}=="2dc8", MODE="0666"
'';
}

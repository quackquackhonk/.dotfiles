{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    prismlauncher
    retroarch-full
  ];
}

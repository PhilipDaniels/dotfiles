#!/bin/sh
# Base16 Embers - Virtual console color setup script
# Jannik Siebert (https://github.com/janniks)

color00="16130F" # Base 00 - Black
color01="826D57" # Base 08 - Red
color02="57826D" # Base 0B - Green
color03="6D8257" # Base 0A - Yellow
color04="6D5782" # Base 0D - Blue
color05="82576D" # Base 0E - Magenta
color06="576D82" # Base 0C - Cyan
color07="A39A90" # Base 05 - White
color08="5A5047" # Base 03 - Bright Black
color09=$color01 # Base 08 - Bright Red
color10=$color02 # Base 0B - Bright Green
color11=$color03 # Base 0A - Bright Yellow
color12=$color04 # Base 0D - Bright Blue
color13=$color05 # Base 0E - Bright Magenta
color14=$color06 # Base 0C - Bright Cyan
color15="DBD6D1" # Base 07 - Bright White

# 16 color space
echo -e "\e]P0$color00"
echo -e "\e]P1$color01"
echo -e "\e]P2$color02"
echo -e "\e]P3$color03"
echo -e "\e]P4$color04"
echo -e "\e]P5$color05"
echo -e "\e]P6$color06"
echo -e "\e]P7$color07"
echo -e "\e]P8$color08"
echo -e "\e]P9$color09"
echo -e "\e]PA$color10"
echo -e "\e]PB$color11"
echo -e "\e]PC$color12"
echo -e "\e]PD$color13"
echo -e "\e]PE$color14"
echo -e "\e]PF$color15"
echo -e "\e[H"
echo -e "\e[2J"

# clean up
unset color00
unset color01
unset color02
unset color03
unset color04
unset color05
unset color06
unset color07
unset color08
unset color09
unset color10
unset color11
unset color12
unset color13
unset color14
unset color15

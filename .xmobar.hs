Config { font = "-xos4-terminus-medium-r-*-*-12-*-*-*-*-*-*-" -- "xft:Terminus (TTF):size=9"
       , bgColor = "black"
       , fgColor = "grey"
       , position = BottomW L 100
       , lowerOnStart = True
       , commands = [ Run MultiCpu ["-t", "(<total0>, <total1>, <total2>, <total3>)", "-L", "25", "-H", "75", "--normal", "white", "--high", "orange", "-m", "3", "-w", "3", "-S", "true"] 10
                    , Run Memory ["-t", "<used>MB", "-L", "6000", "-H", "15000", "--normal", "white", "--high", "orange", "-c", "0", "-m", "5", "-w", "5"] 10
                    , Run Date "%a %Y %b %d %R" "date" 10
                    , Run CommandReader "while true; do acpi -b | awk '{print $4, $3, $5}' | sed -e 's/,//g' -e s/100/FF/ -e s/Discharging/D/ -e s/Charging/C/ -e 's/Unknown /F UU:UU:UU/' -e 's/Full /F FF:FF:FF/' -e 's/until/UU:UU:UU/' | tr '\n' ' '; echo; sleep 10; done" "battery"
--                    , Run BatteryP ["BAT0"] ["-t", "ACSTATUS: <acstatus>, WATTS: <watts>, TIMELEFT: <timeleft>, LEFTBAR: <leftbar>, LEFT: <left>"] 100
                    , Run StdinReader
                    , Run CommandReader "exec /home/bos/rharwood/.mpdmonitor.sh xmobar" "MPD"
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%date% │ %memory% │ %battery%│ %multicpu% │ %StdinReader% }{ %MPD%"
       }

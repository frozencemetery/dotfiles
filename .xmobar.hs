-- monitor scripts: https://github.com/frozencemetery/monitors

Config { font = "Terminus 8"
       , bgColor = "black"
       , fgColor = "grey"
       , position = BottomW L 100
       , lowerOnStart = True
       , commands = [ Run MultiCpu ["-t", "(<total0>, <total1>, <total2>, <total3>, <total4>, <total5>, <total6>, <total7>, <total8>, <total9>, <total10>, <total11>)", "-L", "25", "-H", "75", "--normal", "white", "--high", "orange", "-m", "3", "-w", "3", "-S", "true"] 10
                    , Run Memory ["-t", "<used>MB", "-L", "6000", "-H", "15000", "--normal", "white", "--high", "orange", "-c", "0", "-m", "5", "-w", "5"] 10
                    , Run Date "%a %Y %b %d %R" "date" 10
                    , Run CommandReader "exec batterymonitor.py" "battery"
                    , Run StdinReader
                    , Run CommandReader "exec mpdmonitor.py" "MPD"
                    , Run CommandReader "exec alsamonitor.py" "alsa"
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%date% │ %memory% │ %battery% │ %multicpu% │ %StdinReader% }{ %MPD% %alsa%"
       }

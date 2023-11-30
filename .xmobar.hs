-- monitor scripts: https://github.com/frozencemetery/monitors

Config { font = "Terminus 8"
       , bgColor = "black"
       , fgColor = "lightgrey"
       , position = BottomW L 100
       , lowerOnStart = True
       , commands =
         [ Run MultiCpu ["-t", "(<total0>, <total1>, <total2>, <total3>)",
                         "-L", "25", "-H", "75", "--normal", "white",
                         "--high", "orange", "-m", "3", "-w", "3",
                         "-S", "true"] 10
         , Run Memory ["-t", "<used>MB", "-L", "6000", "-H", "15000",
                       "--normal", "white", "--high", "orange", "-c", "0",
                       "-m", "5", "-w", "5"] 10
         , Run Date "%a %Y %b %d %R" "date" 10
         , Run StdinReader

         -- monitor scripts: https://github.com/frozencemetery/monitors
         , Run CommandReader "exec batterymonitor.py" "battery"
         , Run CommandReader "exec mpdmonitor.py" "MPD"
         , Run CommandReader "exec alsamonitor.py" "alsa"
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "}%date% │ %memory% │ %battery% │ %multicpu% │ %StdinReader% { %MPD% %alsa%"
       }

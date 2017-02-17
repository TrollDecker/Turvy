Attribute VB_Name = "Clocks"
Dim TimerString As String

Public Sub IncreaseClock()
    'Always tick up the milliseconds by five per frame.
    LevelMilliseconds = LevelMilliseconds + 5
    
    'When 100 milliseconds pass, a second passes.
    If LevelMilliseconds >= 100 Then
        LevelMilliseconds = 0
        LevelSeconds = LevelSeconds + 1
    End If
    
    'When 60 seconds pass, a minute passes.
    If LevelSeconds >= 60 Then
        LevelSeconds = 0
        LevelMinutes = LevelMinutes + 1
    End If
    
    'Restrict the timer to prevent an error (even though it would take 45 days to reach 65536 minutes).
    If LevelMinutes >= 10000 Then
        LevelMinutes = 10000
        LevelSeconds = 0
        LevelMilliseconds = 0
    End If
    
    'Construct a string containing the current timer.
    'If any of the unit values are below 10, then add a leading zero to keep them 2 characters long.
    TimerString = "Timer: "
    If LevelMinutes < 10 Then
        TimerString = TimerString & "0" & LevelMinutes & ":"
    Else
        TimerString = TimerString & LevelMinutes & ":"
    End If
    
    If LevelSeconds < 10 Then
        TimerString = TimerString & "0" & LevelSeconds & ":"
    Else
        TimerString = TimerString & LevelSeconds & ":"
    End If
    
    If LevelMilliseconds < 10 Then
        TimerString = TimerString & "0" & LevelMilliseconds
    Else
        TimerString = TimerString & LevelMilliseconds
    End If
    
    'Send the constructed string to the timer label.
    frmGravity.lblTimer.Caption = TimerString
    
End Sub

Public Sub ResetClock()
    LevelMinutes = 0
    LevelSeconds = 0
    LevelMilliseconds = 0
End Sub

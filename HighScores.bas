Attribute VB_Name = "HighScores"
Dim TopName(0 To 5) As String
Dim TopMinute(0 To 5) As Integer
Dim TopSecond(0 To 5) As Integer
Dim TopMillisecond(0 To 5) As Integer
Dim TempName As String
Dim TempMinute, TempSecond, TempMillisecond As Integer

Public Sub WriteScore()
    
    Dim TotalMilliseconds(0 To 1) As Single
    Dim LineCounter As Integer
    
    Open "Scores.txt" For Input As #1
    LineCounter = 0
    
    Do While Not EOF(1)
        Input #1, TopName(LineCounter), TopMinute(LineCounter), TopSecond(LineCounter), TopMillisecond(LineCounter)
        LineCounter = LineCounter + 1
    Loop
    
    Close #1
    
    TopName(5) = PlayerName
    TopMinute(5) = LevelMinutes
    TopSecond(5) = LevelSeconds
    TopMillisecond(5) = LevelMilliseconds
    
    For LineCounter = 5 To 1 Step -1
        TotalMilliseconds(0) = ((((TopMinute(LineCounter - 1) / 100000000) * 60 + (TopSecond(LineCounter - 1) / 100000000)) * 100) + (TopMillisecond(LineCounter - 1)) / 100000000)
        TotalMilliseconds(1) = ((((TopMinute(LineCounter) / 100000000) * 60 + (TopSecond(LineCounter) / 100000000)) * 100) + (TopMillisecond(LineCounter)) / 100000000)
        
        If TotalMilliseconds(1) < TotalMilliseconds(0) Then
            Call MoveName(LineCounter)
        End If
            
    Next LineCounter
    
    Open "Scores.txt" For Output As #2
    For LineCounter = 0 To 4
        Write #2, TopName(LineCounter), TopMinute(LineCounter), TopSecond(LineCounter), TopMillisecond(LineCounter)
    Next LineCounter
    
    Close #2
End Sub

Public Sub MoveName(ScoreLine As Integer)
    TempName = TopName(ScoreLine - 1)
    TempMinute = TopMinute(ScoreLine - 1)
    TempSecond = TopSecond(ScoreLine - 1)
    TempMillisecond = TopMillisecond(ScoreLine - 1)
    
    TopName(ScoreLine - 1) = TopName(ScoreLine)
    TopMinute(ScoreLine - 1) = TopMinute(ScoreLine)
    TopSecond(ScoreLine - 1) = TopSecond(ScoreLine)
    TopMillisecond(ScoreLine - 1) = TopMillisecond(ScoreLine)
    
    TopName(ScoreLine) = TempName
    TopMinute(ScoreLine) = TempMinute
    TopSecond(ScoreLine) = TempSecond
    TopMillisecond(ScoreLine) = TempMillisecond
End Sub

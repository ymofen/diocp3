object FMMonitor: TFMMonitor
  Left = 0
  Top = 0
  Width = 483
  Height = 225
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 63
    Height = 13
    Caption = 'server state:'
  end
  object lblsvrState: TLabel
    Left = 96
    Top = 16
    Width = 51
    Height = 13
    Caption = 'lblsvrState'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 25
    Height = 13
    Caption = 'recv:'
  end
  object lblrecvState: TLabel
    Left = 96
    Top = 48
    Width = 57
    Height = 13
    Caption = 'lblrecvState'
  end
  object Label3: TLabel
    Left = 16
    Top = 67
    Width = 27
    Height = 13
    Caption = 'send:'
  end
  object lblSend: TLabel
    Left = 96
    Top = 67
    Width = 34
    Height = 13
    Caption = 'lblSend'
  end
  object Label4: TLabel
    Left = 18
    Top = 136
    Width = 48
    Height = 13
    Caption = 'acceptex:'
  end
  object lblAcceptEx: TLabel
    Left = 98
    Top = 136
    Width = 55
    Height = 13
    Caption = 'lblAcceptEx'
  end
  object lblOnlineCounter: TLabel
    Left = 98
    Top = 156
    Width = 79
    Height = 13
    Caption = 'lblOnlineCounter'
  end
  object Label5: TLabel
    Left = 18
    Top = 156
    Width = 32
    Height = 13
    Caption = 'online:'
  end
  object lblRunTimeINfo: TLabel
    Left = 98
    Top = 196
    Width = 72
    Height = 13
    Caption = 'lblRunTimeINfo'
  end
  object Label6: TLabel
    Left = 18
    Top = 175
    Width = 42
    Height = 13
    Caption = 'workers:'
  end
  object lblWorkerCount: TLabel
    Left = 98
    Top = 175
    Width = 74
    Height = 13
    Caption = 'lblWorkerCount'
  end
  object Label7: TLabel
    Left = 18
    Top = 196
    Width = 43
    Height = 13
    Caption = 'run time:'
  end
  object tmrReader: TTimer
    Enabled = False
    OnTimer = tmrReaderTimer
    Left = 416
    Top = 24
  end
end

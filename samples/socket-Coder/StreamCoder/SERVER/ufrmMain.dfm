object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'diocp3 server for diocp1'
  ClientHeight = 357
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object edtPort: TEdit
    Left = 9
    Top = 7
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '9983'
  end
  object btnOpen: TButton
    Left = 142
    Top = 4
    Width = 75
    Height = 25
    Action = actOpen
    TabOrder = 1
  end
  object edtMsg: TEdit
    Left = 256
    Top = 9
    Width = 232
    Height = 21
    TabOrder = 2
    Text = 'this message will push to all client'
  end
  object btnPushMsg: TButton
    Left = 494
    Top = 8
    Width = 75
    Height = 25
    Action = actPushMsg
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 36
    Width = 566
    Height = 313
    ActivePage = TabSheet2
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 558
        Height = 285
        Align = alClient
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = -112
        ExplicitTop = -15
        ExplicitWidth = 561
        ExplicitHeight = 297
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'log'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 558
        Height = 285
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 160
        ExplicitTop = 88
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
  end
  object actlstMain: TActionList
    Left = 232
    Top = 272
    object actOpen: TAction
      Caption = 'start'
      OnExecute = actOpenExecute
    end
    object actStop: TAction
      Caption = 'stop'
      OnExecute = actStopExecute
    end
    object actPushMsg: TAction
      Caption = 'PushMsg'
      OnExecute = actPushMsgExecute
    end
  end
end

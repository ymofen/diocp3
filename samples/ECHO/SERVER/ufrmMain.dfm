object frmMain: TfrmMain
  Left = 391
  Top = 275
  BorderIcons = [biSystemMenu]
  Caption = 'diocp3 echo server'
  ClientHeight = 387
  ClientWidth = 661
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
  object btnDisconectAll: TButton
    Left = 232
    Top = 4
    Width = 113
    Height = 25
    Caption = 'btnDisconectAll'
    TabOrder = 2
    OnClick = btnDisconectAllClick
  end
  object pgcMain: TPageControl
    Left = 8
    Top = 35
    Width = 645
    Height = 350
    ActivePage = TabSheet1
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'moniter'
      ExplicitWidth = 617
      ExplicitHeight = 302
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 637
        Height = 322
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 617
        ExplicitHeight = 302
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'log'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 617
        Height = 302
        Align = alClient
        Lines.Strings = (
          'mmoLog')
        TabOrder = 0
      end
    end
  end
  object btnGetWorkerState: TButton
    Left = 360
    Top = 4
    Width = 121
    Height = 25
    Caption = 'btnGetWorkerState'
    TabOrder = 4
    OnClick = btnGetWorkerStateClick
  end
  object btnFindContext: TButton
    Left = 492
    Top = 4
    Width = 94
    Height = 25
    Caption = 'btnFindContext'
    TabOrder = 5
    OnClick = btnFindContextClick
  end
  object actlstMain: TActionList
    Left = 248
    Top = 104
    object actOpen: TAction
      Caption = 'start'
      OnExecute = actOpenExecute
    end
    object actStop: TAction
      Caption = 'stop'
      OnExecute = actStopExecute
    end
  end
end

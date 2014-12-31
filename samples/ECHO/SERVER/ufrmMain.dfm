object frmMain: TfrmMain
  Left = 391
  Top = 275
  BorderIcons = [biSystemMenu]
  Caption = 'diocp3 echo server'
  ClientHeight = 387
  ClientWidth = 696
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
  object pgcMain: TPageControl
    Left = 0
    Top = 41
    Width = 696
    Height = 346
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 661
    object TabSheet1: TTabSheet
      Caption = 'moniter'
      ExplicitWidth = 653
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 688
        Height = 318
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 653
      end
    end
    object tsLog: TTabSheet
      Caption = 'log'
      ImageIndex = 1
      ExplicitWidth = 653
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 688
        Height = 318
        Align = alClient
        Lines.Strings = (
          'mmoLog')
        TabOrder = 0
        ExplicitWidth = 653
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 696
    Height = 41
    Align = alTop
    Caption = 'pnlTop'
    TabOrder = 1
    ExplicitWidth = 661
    object btnDisconectAll: TButton
      Left = 232
      Top = 5
      Width = 113
      Height = 25
      Caption = 'btnDisconectAll'
      TabOrder = 0
      OnClick = btnDisconectAllClick
    end
    object btnFindContext: TButton
      Left = 478
      Top = 5
      Width = 94
      Height = 25
      Caption = 'btnFindContext'
      TabOrder = 1
      OnClick = btnFindContextClick
    end
    object btnGetWorkerState: TButton
      Left = 351
      Top = 5
      Width = 121
      Height = 25
      Caption = 'btnGetWorkerState'
      TabOrder = 2
      OnClick = btnGetWorkerStateClick
    end
    object btnOpen: TButton
      Left = 142
      Top = 5
      Width = 75
      Height = 25
      Action = actOpen
      TabOrder = 3
    end
    object edtPort: TEdit
      Left = 9
      Top = 7
      Width = 121
      Height = 21
      TabOrder = 4
      Text = '9983'
    end
    object btnPostWSAClose: TButton
      Left = 578
      Top = 5
      Width = 103
      Height = 25
      Caption = 'btnPostWSAClose'
      TabOrder = 5
      OnClick = btnPostWSACloseClick
    end
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

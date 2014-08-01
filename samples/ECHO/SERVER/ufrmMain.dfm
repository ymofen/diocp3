object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'diocp3 echo server'
  ClientHeight = 373
  ClientWidth = 626
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
    Left = 248
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
    Width = 610
    Height = 330
    ActivePage = TabSheet1
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'moniter'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 602
        Height = 302
        Align = alClient
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = -168
        ExplicitTop = -143
        ExplicitWidth = 449
        ExplicitHeight = 308
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
        Width = 602
        Height = 302
        Align = alClient
        Lines.Strings = (
          'mmoLog')
        TabOrder = 0
        ExplicitLeft = 15
        ExplicitTop = -134
        ExplicitWidth = 250
        ExplicitHeight = 307
      end
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

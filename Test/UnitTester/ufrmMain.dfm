object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 471
  ClientWidth = 829
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 47
    Width = 604
    Height = 416
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 128
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 296
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 3
    OnClick = Button3Click
  end
  object btnDoublyLinked: TButton
    Left = 472
    Top = 16
    Width = 100
    Height = 25
    Caption = 'btnDoublyLinked'
    TabOrder = 4
    OnClick = btnDoublyLinkedClick
  end
  object btnDoublyLinkedConsume: TButton
    Left = 600
    Top = 16
    Width = 121
    Height = 25
    Caption = 'btnDoublyLinkedConsume'
    TabOrder = 5
    OnClick = btnDoublyLinkedConsumeClick
  end
  object btnBaseQueue: TButton
    Left = 632
    Top = 112
    Width = 145
    Height = 25
    Caption = 'btnBaseQueue'
    TabOrder = 6
    OnClick = btnBaseQueueClick
  end
  object btnBaseQueueConsume: TButton
    Left = 632
    Top = 160
    Width = 145
    Height = 25
    Caption = 'btnBaseQueueConsume'
    TabOrder = 7
    OnClick = btnBaseQueueConsumeClick
  end
  object Button4: TButton
    Left = 632
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 8
    OnClick = Button4Click
  end
end

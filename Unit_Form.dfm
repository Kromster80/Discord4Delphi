object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    625
    441)
  TextHeight = 15
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Init Discord'
    TabOrder = 0
    OnClick = Button1Click
  end
  object meLog: TMemo
    Left = 136
    Top = 8
    Width = 481
    Height = 425
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    ExplicitWidth = 471
    ExplicitHeight = 405
  end
  object Button2: TButton
    Left = 8
    Top = 40
    Width = 121
    Height = 25
    Caption = 'Change activity'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 72
    Width = 121
    Height = 25
    Caption = 'Clear activity'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 152
    Top = 24
  end
end

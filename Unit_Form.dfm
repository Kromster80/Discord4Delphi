object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Discord4Delphi test app'
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
  object meLog: TMemo
    Left = 136
    Top = 8
    Width = 481
    Height = 425
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 471
    ExplicitHeight = 405
  end
  object btnChangeActivity: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Change activity'
    TabOrder = 1
    OnClick = btnChangeActivityClick
  end
  object bynClearActivity: TButton
    Left = 8
    Top = 40
    Width = 121
    Height = 25
    Caption = 'Clear activity'
    TabOrder = 2
    OnClick = bynClearActivityClick
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 152
    Top = 24
  end
end

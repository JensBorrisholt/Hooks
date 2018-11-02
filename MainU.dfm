object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'FormMain'
  ClientHeight = 186
  ClientWidth = 256
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
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Activate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 241
    Height = 138
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
end

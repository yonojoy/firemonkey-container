object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TFireMonkeyContainer fullsize demo'
  ClientHeight = 340
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object FireMonkeyContainer: TFireMonkeyContainer
    Left = 0
    Top = 0
    Width = 680
    Height = 340
    OnCreateFMXForm = FireMonkeyContainerCreateFMXForm
    OnDestroyFMXForm = FireMonkeyContainerDestroyFMXForm
    Align = alClient
    ExplicitLeft = 2
    ExplicitTop = 2
    ExplicitWidth = 427
    ExplicitHeight = 271
  end
end

object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DungenGUI'
  ClientHeight = 576
  ClientWidth = 576
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SeedLabel: TLabel
    Left = 8
    Top = 362
    Width = 28
    Height = 13
    Caption = 'Seed:'
  end
  object WidthLabel: TLabel
    Left = 11
    Top = 399
    Width = 32
    Height = 13
    Caption = 'Width:'
  end
  object HeightLabel: TLabel
    Left = 8
    Top = 426
    Width = 35
    Height = 13
    Caption = 'Height:'
  end
  object MinSizeLabel: TLabel
    Left = 96
    Top = 399
    Width = 96
    Height = 13
    Alignment = taRightJustify
    Caption = 'Minimum Room Size:'
  end
  object depthLabel: TLabel
    Left = 86
    Top = 426
    Width = 106
    Height = 13
    Alignment = taRightJustify
    Caption = 'Recursive Depth:'
  end
  object TextBox: TMemo
    Left = 8
    Top = 8
    Width = 560
    Height = 345
    Lines.Strings = (
      'TextBox')
    TabOrder = 0
  end
  object SeedTextBox: TEdit
    Left = 42
    Top = 359
    Width = 239
    Height = 21
    Alignment = taRightJustify
    TabOrder = 1
  end
  object WidthTextBox: TEdit
    Left = 49
    Top = 396
    Width = 31
    Height = 21
    TabOrder = 2
    Text = '80'
  end
  object HeightTextBox: TEdit
    Left = 49
    Top = 423
    Width = 31
    Height = 21
    TabOrder = 3
    Text = '80'
  end
  object MinRoomSizeTextBox: TEdit
    Left = 206
    Top = 396
    Width = 19
    Height = 21
    TabOrder = 4
    Text = '4'
  end
  object depthTextBox: TEdit
    Left = 206
    Top = 423
    Width = 19
    Height = 21
    TabOrder = 5
    Text = '4'
  end
  object MainMenu: TMainMenu
    Left = 504
    Top = 65528
    object FileMenu: TMenuItem
      Caption = 'File'
      object NewFile: TMenuItem
        Caption = 'New'
      end
      object LoadFile: TMenuItem
        Caption = 'Load'
      end
      object ExitProgram: TMenuItem
        Caption = 'Exit'
      end
    end
  end
end

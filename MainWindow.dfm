object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DungenGUI'
  ClientHeight = 445
  ClientWidth = 576
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SeedLabel: TLabel
    Left = 8
    Top = 364
    Width = 28
    Height = 13
    Caption = 'Seed:'
  end
  object WidthLabel: TLabel
    Left = 11
    Top = 390
    Width = 32
    Height = 13
    Caption = 'Width:'
  end
  object HeightLabel: TLabel
    Left = 8
    Top = 414
    Width = 35
    Height = 13
    Caption = 'Height:'
  end
  object MinSizeLabel: TLabel
    Left = 96
    Top = 390
    Width = 96
    Height = 13
    Alignment = taRightJustify
    Caption = 'Minimum Room Size:'
  end
  object depthLabel: TLabel
    Left = 109
    Top = 414
    Width = 83
    Height = 13
    Alignment = taRightJustify
    Caption = 'Recursive Depth:'
  end
  object SplitVarLabel: TLabel
    Left = 244
    Top = 364
    Width = 68
    Height = 13
    Caption = 'Split Variance:'
  end
  object SizeVarLabel: TLabel
    Left = 245
    Top = 390
    Width = 67
    Height = 13
    Caption = 'Size Variance:'
  end
  object DoorVarLabel: TLabel
    Left = 241
    Top = 414
    Width = 71
    Height = 13
    Caption = 'Door Variance:'
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
    Width = 95
    Height = 25
    Alignment = taRightJustify
    TabOrder = 1
    OnChange = SeedTextBoxChange
  end
  object WidthTextBox: TEdit
    Left = 49
    Top = 387
    Width = 31
    Height = 21
    TabOrder = 2
    Text = '80'
  end
  object HeightTextBox: TEdit
    Left = 49
    Top = 414
    Width = 31
    Height = 21
    TabOrder = 3
    Text = '80'
  end
  object MinRoomSizeTextBox: TEdit
    Left = 206
    Top = 387
    Width = 19
    Height = 21
    TabOrder = 4
    Text = '4'
  end
  object depthTextBox: TEdit
    Left = 206
    Top = 414
    Width = 19
    Height = 21
    TabOrder = 5
    Text = '4'
  end
  object NewSeedButton: TButton
    Left = 143
    Top = 359
    Width = 75
    Height = 25
    Caption = 'New Seed'
    TabOrder = 6
    OnClick = NewSeedButtonClick
  end
  object SplitVarTextBox: TEdit
    Left = 318
    Top = 359
    Width = 27
    Height = 21
    TabOrder = 7
    Text = '0.5'
  end
  object SizeVarTextBox: TEdit
    Left = 318
    Top = 386
    Width = 27
    Height = 21
    TabOrder = 8
    Text = '0.5'
  end
  object DoorVarTextBox: TEdit
    Left = 318
    Top = 413
    Width = 27
    Height = 21
    TabOrder = 9
    Text = '0.5'
  end
  object GenerateButton: TButton
    Left = 360
    Top = 359
    Width = 208
    Height = 76
    Caption = 'Generate'
    TabOrder = 10
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
      object SaveFile: TMenuItem
        Caption = 'Save'
      end
      object ExitProgram: TMenuItem
        Caption = 'Exit'
      end
    end
  end
end

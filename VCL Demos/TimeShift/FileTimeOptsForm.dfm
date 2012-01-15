object frmFileTimeOptions: TfrmFileTimeOptions
  Left = 246
  Top = 92
  BorderStyle = bsDialog
  Caption = 'File Date/Times'
  ClientHeight = 147
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object rdoPreserve: TRadioButton
    Left = 16
    Top = 16
    Width = 181
    Height = 17
    Caption = '&Preserve what they were before'
    TabOrder = 0
  end
  object rdoMatchExif: TRadioButton
    Left = 16
    Top = 44
    Width = 281
    Height = 17
    Caption = '&Update them to match their respective Exif value'
    TabOrder = 1
  end
  object rdoSetToNow: TRadioButton
    Left = 16
    Top = 67
    Width = 305
    Height = 33
    Caption = 
      'Update them to match the &current time - i.e., don'#39't lie about w' +
      'hen the processed files were '#39'last modified'#39
    TabOrder = 2
    WordWrap = True
  end
  object Button1: TButton
    Left = 167
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 248
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end

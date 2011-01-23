object frmSaveTest: TfrmSaveTest
  Left = 246
  Top = 92
  BorderStyle = bsDialog
  Caption = 'TExifData Saving Code Tester'
  ClientHeight = 517
  ClientWidth = 327
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
  object grpSourceFile: TGroupBox
    Left = 8
    Top = 8
    Width = 310
    Height = 48
    Caption = ' Source image file '
    TabOrder = 0
    object btnChangeSourceImage: TSpeedButton
      Left = 279
      Top = 16
      Width = 24
      Height = 24
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FF078DBE
        078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
        BE078DBEFF00FFFF00FF078DBE25A1D172C7E785D7FA66CDF965CDF965CDF965
        CDF965CDF865CDF965CDF866CEF939ADD8078DBEFF00FFFF00FF078DBE4CBCE7
        39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
        D984D7EB078DBEFF00FF078DBE72D6FA078DBEAEEAFC79DCFB79DCFB79DCFB79
        DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9AEF1F9078DBEFF00FF078DBE79DDFB
        1899C79ADFF392E7FB84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
        DAB3F4F9078DBEFF00FF078DBE82E3FC43B7DC65C3E0ACF0FD8DEBFC8DEBFC8D
        EBFD8DEBFD8DEBFC8DEBFD0C85184CBBDAB6F7F96DCAE0078DBE078DBE8AEAFC
        77DCF3229CC6FDFFFFC8F7FEC9F7FEC9F7FEC9F7FEC8F7FE0C85183CBC5D0C85
        18DEF9FBD6F6F9078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
        8DBE078DBE0C851852D97F62ED9741C4650C8518078DBE078DBE078DBE9BF5FE
        9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE0C851846CE6C59E48858E18861EB
        9440C1650C8518FF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFE0C
        85180C85180C85180C851856E18447CD6E0C85180C85180C8518FF00FF078DBE
        FEFEFEA5FEFFA5FEFFA5FEFF078CB643B7DC43B7DC43B7DC0C85184EDD7936BA
        540C8518FF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
        00FFFF00FFFF00FF0C851840D0650C8518FF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85182AB7432DBA490C85
        18FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FF0C851821B5380C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FF0C85180C85180C85180C8518FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85180C85180C
        85180C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      OnClick = btnChangeSourceImageClick
    end
    object edtSourceFile: TEdit
      Left = 9
      Top = 17
      Width = 271
      Height = 21
      TabOrder = 0
      OnChange = edtSourceFileChange
      OnDblClick = edtSourceFileDblClick
      OnKeyDown = edtSourceFileKeyDown
      OnKeyPress = edtSourceFileKeyPress
    end
  end
  object grpExif: TGroupBox
    Left = 8
    Top = 61
    Width = 309
    Height = 252
    Caption = ' Exif '
    TabOrder = 1
    object chkClearGeneralSection: TCheckBox
      Left = 11
      Top = 87
      Width = 158
      Height = 17
      Caption = 'Remove general TIFF tags'
      TabOrder = 3
    end
    object chkClearDetailsSection: TCheckBox
      Left = 11
      Top = 110
      Width = 172
      Height = 17
      Caption = 'Remove Exif-specific tags'
      TabOrder = 4
    end
    object chkRemoveMakerNoteTag: TCheckBox
      Left = 26
      Top = 133
      Width = 188
      Height = 17
      Caption = 'Remove MakerNote tag, if exists'
      TabOrder = 5
    end
    object chkSwitchByteOrder: TCheckBox
      Left = 11
      Top = 41
      Width = 114
      Height = 17
      Caption = 'Switch byte order'
      TabOrder = 1
    end
    object chkRemovePaddingTags: TCheckBox
      Left = 11
      Top = 64
      Width = 290
      Height = 17
      Caption = 'Remove any padding tags added by MS software'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkRemoveExifSegment: TCheckBox
      Left = 11
      Top = 18
      Width = 173
      Height = 17
      Caption = 'Remove all Exif data'
      TabOrder = 0
      OnClick = chkRemoveExifSegmentClick
    end
    object chkRemoveInteropSection: TCheckBox
      Left = 11
      Top = 156
      Width = 190
      Height = 17
      Caption = 'Remove any interop section'
      TabOrder = 6
    end
    object chkRemoveGPSSection: TCheckBox
      Left = 11
      Top = 179
      Width = 178
      Height = 17
      Caption = 'Remove any GPS section'
      TabOrder = 7
    end
    object chkRemoveThumbnail: TCheckBox
      Left = 11
      Top = 202
      Width = 203
      Height = 17
      Caption = 'Remove any thumbnail'
      TabOrder = 8
      OnClick = chkRemoveThumbnailClick
    end
    object chkSetDummyThumbnail: TCheckBox
      Left = 26
      Top = 225
      Width = 135
      Height = 17
      Caption = 'Set dummy thumbnail '
      TabOrder = 9
    end
  end
  object rdgXMP: TRadioGroup
    Left = 8
    Top = 318
    Width = 309
    Height = 107
    Caption = ' XMP '
    ItemIndex = 1
    Items.Strings = (
      'Remove all XMP data'
      'Preserve any existing XMP data'
      'Rewrite XMP data, sync'#39'ing to edited Exif data'
      'Rewrite XMP data, sync'#39'ing to original Exif data')
    TabOrder = 2
  end
  object btnCreateNewImage: TButton
    Left = 93
    Top = 484
    Width = 145
    Height = 25
    Caption = 'Create New Image File'
    Default = True
    Enabled = False
    TabOrder = 4
    OnClick = btnCreateNewImageClick
  end
  object btnClose: TButton
    Left = 244
    Top = 484
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Exit'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object rdgClassToTest: TRadioGroup
    Left = 8
    Top = 431
    Width = 309
    Height = 45
    Caption = ' Class to test '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'TExifData directly'
      'TJpegImageEx')
    TabOrder = 3
  end
  object dlgSelectSource: TOpenPictureDialog
    DefaultExt = 'jpg'
    Filter = 'JPEG Image Files (*.jpg, *.jpeg)|*.jpg;*.jpeg'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 200
    Top = 80
  end
  object dlgDest: TSaveDialog
    DefaultExt = 'jpg'
    Filter = 'JPEG Image Files (*.jpg)|*.jpg|JPEG Image Files (*.jpeg)|*.jpeg'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 236
    Top = 80
  end
end

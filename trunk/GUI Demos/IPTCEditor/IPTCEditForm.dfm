object frmIPTC: TfrmIPTC
  Left = 252
  Top = 95
  Caption = 'IPTC Tag Editor'
  ClientHeight = 519
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  OldCreateOrder = False
  ScreenSnap = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 17
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 389
    Height = 475
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 10
    ExplicitTop = 10
    ExplicitWidth = 385
    ExplicitHeight = 471
    object TabSheet1: TTabSheet
      Caption = 'Envelope Record'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object scbEnvelope: TScrollBox
        Left = 0
        Top = 0
        Width = 381
        Height = 443
        Align = alClient
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 0
        ExplicitWidth = 374
        ExplicitHeight = 429
        DesignSize = (
          381
          443)
        object Label1: TLabel
          Left = 8
          Top = 10
          Width = 83
          Height = 17
          Caption = 'Model version'
          FocusControl = edtModelVersion
        end
        object Label2: TLabel
          Left = 8
          Top = 46
          Width = 68
          Height = 17
          Caption = 'Destination'
          FocusControl = edtDestination
        end
        object Label3: TLabel
          Left = 8
          Top = 81
          Width = 63
          Height = 17
          Caption = 'File format'
          FocusControl = edtFileFormat
        end
        object Label4: TLabel
          Left = 8
          Top = 116
          Width = 111
          Height = 17
          Caption = 'File format version'
          FocusControl = edtFileFormatVersion
        end
        object Label5: TLabel
          Left = 8
          Top = 152
          Width = 98
          Height = 17
          Caption = 'Service identifier'
          FocusControl = edtServiceIdentifier
        end
        object Label6: TLabel
          Left = 8
          Top = 187
          Width = 108
          Height = 17
          Caption = 'Envelope number'
          FocusControl = edtEnvelopeNumber
        end
        object Label7: TLabel
          Left = 8
          Top = 222
          Width = 67
          Height = 17
          Caption = 'Product ID'
          FocusControl = edtProductID
        end
        object Label8: TLabel
          Left = 8
          Top = 258
          Width = 103
          Height = 17
          Caption = 'Envelope priority'
          FocusControl = dtpDateSent
        end
        object Label9: TLabel
          Left = 8
          Top = 293
          Width = 59
          Height = 17
          Caption = 'Date sent'
          FocusControl = dtpDateSent
        end
        object Label10: TLabel
          Left = 8
          Top = 328
          Width = 62
          Height = 17
          Caption = 'UNO code'
          FocusControl = edtUNOCode
        end
        object Label11: TLabel
          Left = 8
          Top = 364
          Width = 81
          Height = 17
          Caption = 'ARM identifier'
          FocusControl = edtARMIdentifier
        end
        object Label12: TLabel
          Left = 8
          Top = 399
          Width = 75
          Height = 17
          Caption = 'ARM version'
          FocusControl = edtARMVersion
        end
        object cboEnvelopePriority: TComboBox
          Left = 194
          Top = 254
          Width = 171
          Height = 25
          Style = csDropDownList
          ItemHeight = 17
          TabOrder = 7
          OnChange = ControlChange
        end
        object edtEnvelopeNumber: TEdit
          Left = 194
          Top = 183
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
          OnChange = ControlChange
        end
        object edtModelVersion: TEdit
          Left = 194
          Top = 7
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = ControlChange
        end
        object edtDestination: TEdit
          Left = 194
          Top = 42
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = ControlChange
        end
        object edtFileFormatVersion: TEdit
          Left = 194
          Top = 112
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          OnChange = ControlChange
        end
        object edtServiceIdentifier: TEdit
          Left = 194
          Top = 148
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          OnChange = ControlChange
        end
        object edtARMVersion: TEdit
          Left = 194
          Top = 395
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 11
          OnChange = ControlChange
        end
        object edtProductID: TEdit
          Left = 194
          Top = 218
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 6
          OnChange = ControlChange
        end
        object dtpDateSent: TDateTimePicker
          Left = 194
          Top = 289
          Width = 171
          Height = 25
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 8
          OnChange = ControlChange
        end
        object edtFileFormat: TEdit
          Left = 194
          Top = 77
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          OnChange = ControlChange
        end
        object edtUNOCode: TEdit
          Left = 194
          Top = 324
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 9
          OnChange = ControlChange
        end
        object edtARMIdentifier: TEdit
          Left = 194
          Top = 360
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 10
          OnChange = ControlChange
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Application Record'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object scbEditorial: TScrollBox
        Left = 0
        Top = 0
        Width = 381
        Height = 443
        Align = alClient
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 0
        ExplicitWidth = 374
        ExplicitHeight = 429
        DesignSize = (
          360
          422)
        object Label23: TLabel
          Left = 8
          Top = 391
          Width = 96
          Height = 17
          Caption = 'Fixture identifier'
          FocusControl = edtFixtureIdentifier
        end
        object Label21: TLabel
          Left = 8
          Top = 289
          Width = 91
          Height = 17
          Caption = 'Category code'
          FocusControl = edtCategoryCode
        end
        object Label19: TLabel
          Left = 8
          Top = 187
          Width = 52
          Height = 17
          Caption = 'Urgency'
          FocusControl = cboUrgency
        end
        object Label17: TLabel
          Left = 8
          Top = 116
          Width = 79
          Height = 17
          Caption = 'Object name'
          FocusControl = edtObjectName
        end
        object Label18: TLabel
          Left = 8
          Top = 150
          Width = 64
          Height = 17
          Caption = 'Edit status'
          FocusControl = edtEditStatus
        end
        object Label13: TLabel
          Left = 8
          Top = 10
          Width = 92
          Height = 17
          Caption = 'Record version'
          FocusControl = edtRecordVersion
        end
        object Label22: TLabel
          Left = 8
          Top = 326
          Width = 176
          Height = 47
          AutoSize = False
          Caption = 'Supplementary categories (one per line)'
          FocusControl = memSupplementaryCats
          WordWrap = True
        end
        object Label14: TLabel
          Left = 8
          Top = 46
          Width = 134
          Height = 17
          Caption = 'Object type reference'
          FocusControl = edtObjectTypeRef
        end
        object Label20: TLabel
          Left = 8
          Top = 224
          Width = 148
          Height = 53
          AutoSize = False
          Caption = 'Subject references (one per line)'
          FocusControl = memSubjectRefs
          WordWrap = True
        end
        object Label15: TLabel
          Left = 8
          Top = 81
          Width = 158
          Height = 17
          Caption = 'Object attribute reference'
          FocusControl = edtObjectAttributeRef
        end
        object Label16: TLabel
          Left = 8
          Top = 428
          Width = 162
          Height = 53
          AutoSize = False
          Caption = 'Keywords or phrases (one per line)'
          FocusControl = memKeywords
          WordWrap = True
        end
        object Label24: TLabel
          Left = 8
          Top = 496
          Width = 117
          Height = 17
          Caption = 'Content location(s)'
          FocusControl = vleContentLocations
        end
        object Label25: TLabel
          Left = 8
          Top = 602
          Width = 76
          Height = 17
          Caption = 'Release date'
          FocusControl = dtpReleaseDate
        end
        object Label26: TLabel
          Left = 8
          Top = 637
          Width = 92
          Height = 17
          Caption = 'Expiration date'
          FocusControl = dtpExpirationDate
        end
        object Label27: TLabel
          Left = 8
          Top = 671
          Width = 115
          Height = 17
          Caption = 'Special instructions'
          FocusControl = edtSpecialInstructions
        end
        object Label28: TLabel
          Left = 8
          Top = 706
          Width = 88
          Height = 17
          Caption = 'Action advised'
          FocusControl = cboActionAdvised
        end
        object Label29: TLabel
          Left = 8
          Top = 744
          Width = 79
          Height = 17
          Caption = 'Date created'
          FocusControl = dtpDateCreated
        end
        object Label30: TLabel
          Left = 8
          Top = 778
          Width = 120
          Height = 17
          Caption = 'Digital creation date'
          FocusControl = dtpDigitalCreationDate
        end
        object Label31: TLabel
          Left = 8
          Top = 812
          Width = 122
          Height = 17
          Caption = 'Originating program'
          FocusControl = edtOriginatingProgram
        end
        object Label32: TLabel
          Left = 8
          Top = 847
          Width = 101
          Height = 17
          Caption = 'Program version'
          FocusControl = edtProgramVersion
        end
        object Label33: TLabel
          Left = 8
          Top = 883
          Width = 110
          Height = 17
          Caption = 'Object cycle code'
          FocusControl = edtObjectCycleCode
        end
        object Label34: TLabel
          Left = 8
          Top = 921
          Width = 51
          Height = 17
          Caption = 'Byline(s)'
          FocusControl = vleBylines
        end
        object Label35: TLabel
          Left = 8
          Top = 1025
          Width = 24
          Height = 17
          Caption = 'City'
          FocusControl = edtCity
        end
        object Label36: TLabel
          Left = 8
          Top = 1061
          Width = 76
          Height = 17
          Caption = 'Sub-location'
          FocusControl = edtSubLocation
        end
        object Label37: TLabel
          Left = 8
          Top = 1096
          Width = 88
          Height = 17
          Caption = 'Province/state'
          FocusControl = edtProvinceOrState
        end
        object Label38: TLabel
          Left = 8
          Top = 1131
          Width = 85
          Height = 17
          Caption = 'Country code'
          FocusControl = edtCountryCode
        end
        object Label39: TLabel
          Left = 8
          Top = 1166
          Width = 89
          Height = 17
          Caption = 'Country name'
          FocusControl = edtCountryName
        end
        object Label40: TLabel
          Left = 8
          Top = 1202
          Width = 147
          Height = 17
          Caption = 'Original transmission ref.'
          FocusControl = edtOriginalTransmissionRef
        end
        object Label41: TLabel
          Left = 8
          Top = 1237
          Width = 50
          Height = 17
          Caption = 'Headline'
          FocusControl = edtHeadline
        end
        object Label42: TLabel
          Left = 8
          Top = 1272
          Width = 36
          Height = 17
          Caption = 'Credit'
          FocusControl = edtCredit
        end
        object Label43: TLabel
          Left = 8
          Top = 1308
          Width = 43
          Height = 17
          Caption = 'Source'
          FocusControl = edtSource
        end
        object Label44: TLabel
          Left = 8
          Top = 1343
          Width = 102
          Height = 17
          Caption = 'Copyright notice'
          FocusControl = edtCopyrightNotice
        end
        object Label45: TLabel
          Left = 8
          Top = 1380
          Width = 139
          Height = 17
          Caption = 'Contacts (one per line)'
          FocusControl = memContacts
        end
        object Label46: TLabel
          Left = 8
          Top = 1446
          Width = 118
          Height = 17
          Caption = 'Caption or abstract'
          FocusControl = memCaptionOrAbstract
        end
        object Label47: TLabel
          Left = 8
          Top = 1513
          Width = 111
          Height = 50
          AutoSize = False
          Caption = 'Writers/editors (one per line)'
          FocusControl = memWritersOrEditors
          WordWrap = True
        end
        object Label48: TLabel
          Left = 8
          Top = 1578
          Width = 166
          Height = 32
          AutoSize = False
          Caption = 'Image orientation'
          FocusControl = cboImageOrientation
        end
        object edtFixtureIdentifier: TEdit
          Left = 194
          Top = 387
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 9
          OnChange = ControlChange
        end
        object edtObjectTypeRef: TEdit
          Left = 194
          Top = 42
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = ControlChange
        end
        object edtObjectAttributeRef: TEdit
          Left = 194
          Top = 77
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          OnChange = ControlChange
        end
        object edtCategoryCode: TEdit
          Left = 194
          Top = 285
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 7
          OnChange = ControlChange
        end
        object edtRecordVersion: TEdit
          Left = 194
          Top = 7
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = ControlChange
        end
        object edtObjectName: TEdit
          Left = 194
          Top = 112
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          OnChange = ControlChange
        end
        object memSubjectRefs: TMemo
          Left = 194
          Top = 218
          Width = 171
          Height = 59
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 6
          WordWrap = False
          OnChange = ControlChange
        end
        object edtEditStatus: TEdit
          Left = 194
          Top = 148
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          OnChange = ControlChange
        end
        object cboUrgency: TComboBox
          Left = 194
          Top = 183
          Width = 171
          Height = 25
          Style = csDropDownList
          ItemHeight = 17
          TabOrder = 5
          OnChange = ControlChange
        end
        object memSupplementaryCats: TMemo
          Left = 194
          Top = 320
          Width = 171
          Height = 59
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 8
          WordWrap = False
          OnChange = ControlChange
        end
        object memKeywords: TMemo
          Left = 194
          Top = 422
          Width = 171
          Height = 59
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 10
          WordWrap = False
          OnChange = ControlChange
        end
        object vleContentLocations: TValueListEditor
          Left = 194
          Top = 489
          Width = 171
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Ctl3D = True
          KeyOptions = [keyEdit, keyAdd, keyDelete]
          Options = [goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
          ParentCtl3D = False
          TabOrder = 11
          TitleCaptions.Strings = (
            'Code'
            'Name')
          OnStringsChange = ControlChange
          ColWidths = (
            65
            60)
        end
        object dtpReleaseDate: TDateTimePicker
          Left = 194
          Top = 596
          Width = 171
          Height = 25
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 12
          OnChange = ControlChange
        end
        object dtpExpirationDate: TDateTimePicker
          Left = 194
          Top = 632
          Width = 171
          Height = 25
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 13
          OnChange = ControlChange
        end
        object edtSpecialInstructions: TEdit
          Left = 194
          Top = 667
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 14
          OnChange = ControlChange
        end
        object dtpDateCreated: TDateTimePicker
          Left = 194
          Top = 738
          Width = 171
          Height = 25
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 16
          OnChange = ControlChange
        end
        object dtpDigitalCreationDate: TDateTimePicker
          Left = 194
          Top = 773
          Width = 171
          Height = 25
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 17
          OnChange = ControlChange
        end
        object edtOriginatingProgram: TEdit
          Left = 194
          Top = 808
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 18
          OnChange = ControlChange
        end
        object edtProgramVersion: TEdit
          Left = 194
          Top = 843
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 19
          OnChange = ControlChange
        end
        object edtObjectCycleCode: TEdit
          Left = 194
          Top = 879
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 20
          OnChange = ControlChange
        end
        object vleBylines: TValueListEditor
          Left = 194
          Top = 914
          Width = 171
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Ctl3D = True
          KeyOptions = [keyEdit, keyAdd, keyDelete]
          Options = [goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
          ParentCtl3D = False
          TabOrder = 21
          TitleCaptions.Strings = (
            'Name'
            'Title')
          OnStringsChange = ControlChange
          ColWidths = (
            84
            41)
        end
        object edtCity: TEdit
          Left = 194
          Top = 1021
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 22
          OnChange = ControlChange
        end
        object edtSubLocation: TEdit
          Left = 194
          Top = 1057
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 23
          OnChange = ControlChange
        end
        object edtProvinceOrState: TEdit
          Left = 194
          Top = 1092
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 24
          OnChange = ControlChange
        end
        object edtCountryCode: TEdit
          Left = 194
          Top = 1127
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 25
          OnChange = ControlChange
        end
        object edtCountryName: TEdit
          Left = 194
          Top = 1163
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 26
          OnChange = ControlChange
        end
        object edtOriginalTransmissionRef: TEdit
          Left = 194
          Top = 1198
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 27
          OnChange = ControlChange
        end
        object edtHeadline: TEdit
          Left = 194
          Top = 1233
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 28
          OnChange = ControlChange
        end
        object edtCredit: TEdit
          Left = 194
          Top = 1268
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 29
          OnChange = ControlChange
        end
        object edtSource: TEdit
          Left = 194
          Top = 1304
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 30
          OnChange = ControlChange
        end
        object edtCopyrightNotice: TEdit
          Left = 194
          Top = 1339
          Width = 171
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 31
          OnChange = ControlChange
        end
        object memContacts: TMemo
          Left = 194
          Top = 1374
          Width = 171
          Height = 59
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 32
          WordWrap = False
          OnChange = ControlChange
        end
        object memCaptionOrAbstract: TMemo
          Left = 194
          Top = 1441
          Width = 171
          Height = 59
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 33
          OnChange = ControlChange
        end
        object memWritersOrEditors: TMemo
          Left = 194
          Top = 1508
          Width = 171
          Height = 59
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 34
          WordWrap = False
          OnChange = ControlChange
        end
        object cboActionAdvised: TComboBox
          Left = 194
          Top = 702
          Width = 171
          Height = 25
          Style = csDropDownList
          ItemHeight = 17
          TabOrder = 15
          OnChange = ControlChange
          Items.Strings = (
            '<none>'
            'Object kill'
            'Object replace'
            'Object append'
            'Object reference')
        end
        object cboImageOrientation: TComboBox
          Left = 194
          Top = 1574
          Width = 171
          Height = 25
          Style = csDropDownList
          ItemHeight = 17
          TabOrder = 35
          OnChange = ControlChange
          Items.Strings = (
            '<unspecified>'
            'Landscape'
            'Portrait'
            'Square')
        end
      end
    end
  end
  object panActions: TPanel
    Left = 397
    Top = 8
    Width = 122
    Height = 475
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 395
    ExplicitTop = 10
    ExplicitHeight = 471
    object btnOpen: TBitBtn
      Left = 8
      Top = 21
      Width = 115
      Height = 37
      Action = actOpen
      Caption = '&Open File'
      TabOrder = 0
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
    end
    object btnSave: TBitBtn
      Left = 8
      Top = 61
      Width = 115
      Height = 37
      Action = actSaveOrReload
      Caption = '&Save File'
      TabOrder = 1
      Glyph.Data = {
        36060000424D3606000000000000360000002800000020000000100000000100
        18000000000000060000120B0000120B00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        97433F97433FB59A9BB59A9BB59A9BB59A9BB59A9BB59A9BB59A9B9330309743
        3FFF00FFFF00FFFF00FFFF00FFFF00FF8E8E8E8E8E8EBEBEBEBEBEBEBEBEBEBE
        BEBEBEBEBEBEBEBEBEBEBE8686868E8E8EFF00FFFF00FFFF00FFFF00FF97433F
        D66868C66060E5DEDF92292A92292AE4E7E7E0E3E6D9DFE0CCC9CC8F201FAF46
        4697433FFF00FFFF00FFFF00FF8E8E8EB8B8B8AEAEAEECECEC838383838383F0
        F0F0EEEEEEE8E8E8DADADA7E7E7E9A9A9A8E8E8EFF00FFFF00FFFF00FF97433F
        D06566C25F5FE9E2E292292A92292AE2E1E3E2E6E8DDE2E4CFCCCF8F2222AD46
        4697433FFF00FFFF00FFFF00FF8E8E8EB4B4B4ACACACF0F0F0838383838383ED
        EDEDF0F0F0ECECECDCDCDC7F7F7F9999998E8E8EFF00FFFF00FFFF00FF97433F
        D06565C15D5DECE4E492292A92292ADFDDDFE1E6E8E0E5E7D3D0D28A1E1EAB44
        4497433FFF00FFFF00FFFF00FF8E8E8EB4B4B4ABABABF2F2F2838383838383EA
        EAEAEFEFEFEEEEEEE0E0E07C7C7C9898988E8E8EFF00FFFF00FFFF00FF97433F
        D06565C15B5CEFE6E6EDE5E5E5DEDFE0DDDFDFE0E2E0E1E3D6D0D2962A2AB24A
        4A97433FFF00FFFF00FFFF00FF8E8E8EB4B4B4AAAAAAF4F4F4F3F3F3ECECECEA
        EAEAECECECECECECE1E1E18585859D9D9D8E8E8EFF00FFFF00FFFF00FF97433F
        CD6263C86060C96767CC7272CA7271C66969C46464CC6D6CCA6667C55D5DCD65
        6597433FFF00FFFF00FFFF00FF8E8E8EB1B1B1AFAFAFB2B2B2B8B8B8B6B6B6B1
        B1B1AFAFAFB5B5B5B2B2B2ACACACB3B3B38E8E8EFF00FFFF00FFFF00FF97433F
        B65553C27B78D39D9CD7A7A5D8A7A6D8A6A5D7A09FD5A09FD7A9A7D8ABABCC66
        6797433FFF00FFFF00FFFF00FF8E8E8EA2A2A2B6B6B6CBCBCBD0D0D0D1D1D1D0
        D0D0CECECECDCDCDD1D1D1D3D3D3B3B3B38E8E8EFF00FFFF00FFFF00FF97433F
        CC6667F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9CC66
        6797433FFF00FFFF00FFFF00FF8E8E8EB3B3B3FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFB3B3B38E8E8EFF00FFFF00FFFF00FF97433F
        CC6667F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9CC66
        6797433FFF00FFFF00FFFF00FF8E8E8EB3B3B3FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFB3B3B38E8E8EFF00FFFF00FFFF00FF97433F
        CC6667F9F9F9CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDF9F9F9CC66
        6797433FFF00FFFF00FFFF00FF8E8E8EB3B3B3FFFFFFDCDCDCDCDCDCDCDCDCDC
        DCDCDCDCDCDCDCDCDCDCDCFFFFFFB3B3B38E8E8EFF00FFFF00FFFF00FF97433F
        CC6667F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9CC66
        6797433FFF00FFFF00FFFF00FF8E8E8EB3B3B3FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFB3B3B38E8E8EFF00FFFF00FFFF00FF97433F
        CC6667F9F9F9CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDF9F9F9CC66
        6797433FFF00FFFF00FFFF00FF8E8E8EB3B3B3FFFFFFDCDCDCDCDCDCDCDCDCDC
        DCDCDCDCDCDCDCDCDCDCDCFFFFFFB3B3B38E8E8EFF00FFFF00FFFF00FF97433F
        CC6667F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9CC66
        6797433FFF00FFFF00FFFF00FF8E8E8EB3B3B3FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFB3B3B38E8E8EFF00FFFF00FFFF00FFFF00FF
        97433FF9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F99743
        3FFF00FFFF00FFFF00FFFF00FFFF00FF8E8E8EFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF8E8E8EFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      NumGlyphs = 2
    end
    object btnClose: TBitBtn
      Left = 8
      Top = 203
      Width = 115
      Height = 36
      Caption = 'Close'
      TabOrder = 4
      OnClick = btnCloseClick
    end
    object btnClear: TBitBtn
      Left = 8
      Top = 112
      Width = 115
      Height = 37
      Caption = '&Clear All'
      Enabled = False
      TabOrder = 2
      OnClick = btnClearClick
    end
    object btnReload: TBitBtn
      Left = 8
      Top = 153
      Width = 115
      Height = 37
      Action = actSaveOrReload
      Caption = '&Reload File'
      TabOrder = 3
      Glyph.Data = {
        36060000424D3606000000000000360000002800000020000000100000000100
        18000000000000060000120B0000120B00000000000000000000FF00FFFF00FF
        C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
        A4C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3
        B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFC
        FBC2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3FCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCB3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEFCFBFEFCFBFEFCFBFEFCFBD8EBD6018A02018A02D8EBD6FEFCFBFEFC
        FBC2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3FCFCFCFCFCFCFCFCFCFCFCFCE0
        E0E0959595959595E0E0E0FCFCFCFCFCFCB3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEFBF7FEFBF7018A02D8EAD2018A02D8EAD2D8EAD2018A02FEFBF7FEFB
        F7C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3FAFAFAFAFAFA959595DEDEDE95
        9595DEDEDEDEDEDE959595FAFAFAFAFAFAB3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF9F4FEF9F4018A02018A02D8E8D0FEF9F4FEF9F4D8E8D0FEF9F4FEF9
        F4C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3F9F9F9F9F9F9959595959595DC
        DCDCF9F9F9F9F9F9DCDCDCF9F9F9F9F9F9B3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF7F0FEF7F0018A02018A02018A02FEF7F0FEF7F0FEF7F0FEF7F0FEF7
        F0C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3F7F7F7F7F7F795959595959595
        9595F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7B3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF5ECFEF5ECFEF5ECFEF5ECFEF5EC018A02018A02018A02FEF5ECFEF5
        ECC2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3F5F5F5F5F5F5F5F5F5F5F5F5F5
        F5F5959595959595959595F5F5F5F5F5F5B3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF3E9FEF3E9D8E3C7FEF3E9FEF3E9D8E3C7018A02018A02FEF3E9FEF3
        E9C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3F3F3F3F3F3F3D5D5D5F3F3F3F3
        F3F3D5D5D5959595959595F3F3F3F3F3F3B3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFF1E5FFF1E5018A02D9E2C3D9E2C3018A02D9E2C3018A02FFF1E5FFF1
        E5C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3F2F2F2F2F2F2959595D2D2D2D2
        D2D2959595D2D2D2959595F2F2F2F2F2F2B3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFF0E2FFF0E2D9E1C1018A02018A02D9E1C1DDCFC2DDCFC2DDCFC2DDCF
        C2C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3F0F0F0F0F0F0D1D1D195959595
        9595D1D1D1CFCFCFCFCFCFCFCFCFCFCFCFB3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFEEDEFFEEDEFFEEDEFFEEDEFFEEDEFFEEDEC5B5A9C3B4A8C2B3A7C1B2
        A6C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3EEEEEEEEEEEEEEEEEEEEEEEEEE
        EEEEEEEEEEB7B7B7B5B5B5B4B4B4B3B3B3B3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFECDAFFECDAFFECDAFFECDAFFECDAFFECDAB0A296B0A296B0A296B0A2
        96C2A6A4FF00FFFF00FFFF00FFFF00FFB3B3B3ECECECECECECECECECECECECEC
        ECECECECECA3A3A3A3A3A3A3A3A3A3A3A3B3B3B3FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFEAD7FFEAD7FFEAD7FFEAD7FFEAD7C9B9ACFBF8F4FBF8F4E6DAD9C2A6
        A4FF00FFFF00FFFF00FFFF00FFFF00FFB3B3B3EBEBEBEBEBEBEBEBEBEBEBEBEB
        EBEBBABABAF7F7F7F7F7F7DFDFDFB3B3B3FF00FFFF00FFFF00FFFF00FFFF00FF
        C2A6A4FFE8D3FFE8D3FFE8D3FFE8D3FFE8D3C9B9ACFBF8F4DFCEC7C2A6A4FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFB3B3B3E9E9E9E9E9E9E9E9E9E9E9E9E9
        E9E9BABABAF7F7F7D3D3D3B3B3B3FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        C2A6A4FFE6D0FFE6D0FFE6D0FFE6D0FFE6D0C9B9ACDFCEC7C2A6A4FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFB3B3B3E7E7E7E7E7E7E7E7E7E7E7E7E7
        E7E7BABABAD3D3D3B3B3B3FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4FF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFB3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3
        B3B3B3B3B3B3B3B3FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      NumGlyphs = 2
    end
  end
  object panFooter: TPanel
    Left = 8
    Top = 483
    Width = 511
    Height = 28
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4868682
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    Padding.Left = 4
    Padding.Top = 6
    Padding.Right = 4
    ParentFont = False
    TabOrder = 2
    Visible = False
    ExplicitLeft = 10
    ExplicitTop = 481
    ExplicitWidth = 507
    object lblTagsWith: TLabel
      Left = 4
      Top = 6
      Width = 68
      Height = 22
      Align = alLeft
      Caption = 'Tags with '
      ExplicitHeight = 18
    end
    object lblHighlighted: TLabel
      Left = 72
      Top = 6
      Width = 67
      Height = 22
      Align = alLeft
      Caption = 'highlighted'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitHeight = 18
    end
    object lblLabelsHaveEtc: TLabel
      Left = 139
      Top = 6
      Width = 218
      Height = 22
      Align = alLeft
      Caption = ' labels have data in the saved file'
      ExplicitHeight = 18
    end
    object lblNoTagsFound: TLabel
      Left = 5
      Top = 8
      Width = 379
      Height = 19
      AutoSize = False
      Caption = 'No recognised IPTC tags were found in the file'
      Transparent = False
    end
  end
  object ActionList: TActionList
    Left = 336
    Top = 324
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object actOpen: TAction
      Caption = '&Open File'
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
    object actSaveOrReload: TAction
      Caption = '&Save File'
      Enabled = False
      ShortCut = 16467
      OnExecute = actSaveOrReloadExecute
    end
  end
  object dlgOpen: TOpenPictureDialog
    DefaultExt = 'jpg'
    Filter = 
      'All supported image types|*.jpg;*.jpeg;*.psd;*.tif;*.tiff|JPEG i' +
      'mages (*.jpg,*.jpeg)|*.jpg;*.jpeg|Photoshop images (*.psd)|*.psd' +
      '|TIFF images (*.tif, *.tiff)|*.tif;*.tiff'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 304
    Top = 324
  end
end

object frmIPTC: TfrmIPTC
  Left = 239
  Top = 98
  Caption = 'IPTC Tag Editor'
  ClientHeight = 397
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
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
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 294
    Height = 360
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Envelope Record'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object scbEnvelope: TScrollBox
        Left = 0
        Top = 0
        Width = 286
        Height = 332
        Align = alClient
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 0
        DesignSize = (
          286
          332)
        object Label1: TLabel
          Left = 6
          Top = 8
          Width = 66
          Height = 13
          Caption = 'Model version'
          FocusControl = edtModelVersion
        end
        object Label2: TLabel
          Left = 6
          Top = 35
          Width = 54
          Height = 13
          Caption = 'Destination'
          FocusControl = edtDestination
        end
        object Label3: TLabel
          Left = 6
          Top = 62
          Width = 51
          Height = 13
          Caption = 'File format'
          FocusControl = edtFileFormat
        end
        object Label4: TLabel
          Left = 6
          Top = 89
          Width = 89
          Height = 13
          Caption = 'File format version'
          FocusControl = edtFileFormatVersion
        end
        object Label5: TLabel
          Left = 6
          Top = 116
          Width = 80
          Height = 13
          Caption = 'Service identifier'
          FocusControl = edtServiceIdentifier
        end
        object Label6: TLabel
          Left = 6
          Top = 143
          Width = 83
          Height = 13
          Caption = 'Envelope number'
          FocusControl = edtEnvelopeNumber
        end
        object Label7: TLabel
          Left = 6
          Top = 170
          Width = 51
          Height = 13
          Caption = 'Product ID'
          FocusControl = edtProductID
        end
        object Label8: TLabel
          Left = 6
          Top = 197
          Width = 81
          Height = 13
          Caption = 'Envelope priority'
          FocusControl = dtpDateSent
        end
        object Label9: TLabel
          Left = 6
          Top = 224
          Width = 47
          Height = 13
          Caption = 'Date sent'
          FocusControl = dtpDateSent
        end
        object Label10: TLabel
          Left = 6
          Top = 251
          Width = 48
          Height = 13
          Caption = 'UNO code'
          FocusControl = edtUNOCode
        end
        object Label11: TLabel
          Left = 6
          Top = 278
          Width = 67
          Height = 13
          Caption = 'ARM identifier'
          FocusControl = edtARMIdentifier
        end
        object Label12: TLabel
          Left = 6
          Top = 305
          Width = 60
          Height = 13
          Caption = 'ARM version'
          FocusControl = edtARMVersion
        end
        object cboEnvelopePriority: TComboBox
          Left = 148
          Top = 194
          Width = 131
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 7
          OnChange = ControlChange
        end
        object edtEnvelopeNumber: TEdit
          Left = 148
          Top = 140
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
          OnChange = ControlChange
        end
        object edtModelVersion: TEdit
          Left = 148
          Top = 5
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = ControlChange
        end
        object edtDestination: TEdit
          Left = 148
          Top = 32
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = ControlChange
        end
        object edtFileFormatVersion: TEdit
          Left = 148
          Top = 86
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          OnChange = ControlChange
        end
        object edtServiceIdentifier: TEdit
          Left = 148
          Top = 113
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          OnChange = ControlChange
        end
        object edtARMVersion: TEdit
          Left = 148
          Top = 302
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 11
          OnChange = ControlChange
        end
        object edtProductID: TEdit
          Left = 148
          Top = 167
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 6
          OnChange = ControlChange
        end
        object dtpDateSent: TDateTimePicker
          Left = 148
          Top = 221
          Width = 131
          Height = 21
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 8
          OnChange = ControlChange
        end
        object edtFileFormat: TEdit
          Left = 148
          Top = 59
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          OnChange = ControlChange
        end
        object edtUNOCode: TEdit
          Left = 148
          Top = 248
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 9
          OnChange = ControlChange
        end
        object edtARMIdentifier: TEdit
          Left = 148
          Top = 275
          Width = 131
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
        Width = 286
        Height = 332
        Align = alClient
        BorderStyle = bsNone
        ParentBackground = True
        TabOrder = 0
        DesignSize = (
          269
          315)
        object Label23: TLabel
          Left = 6
          Top = 299
          Width = 79
          Height = 13
          Caption = 'Fixture identifier'
          FocusControl = edtFixtureIdentifier
        end
        object Label21: TLabel
          Left = 6
          Top = 221
          Width = 71
          Height = 13
          Caption = 'Category code'
          FocusControl = edtCategoryCode
        end
        object Label19: TLabel
          Left = 6
          Top = 143
          Width = 40
          Height = 13
          Caption = 'Urgency'
          FocusControl = cboUrgency
        end
        object Label17: TLabel
          Left = 6
          Top = 89
          Width = 61
          Height = 13
          Caption = 'Object name'
          FocusControl = edtObjectName
        end
        object Label18: TLabel
          Left = 6
          Top = 115
          Width = 51
          Height = 13
          Caption = 'Edit status'
          FocusControl = edtEditStatus
        end
        object Label13: TLabel
          Left = 6
          Top = 8
          Width = 72
          Height = 13
          Caption = 'Record version'
          FocusControl = edtRecordVersion
        end
        object Label22: TLabel
          Left = 6
          Top = 249
          Width = 135
          Height = 36
          AutoSize = False
          Caption = 'Supplementary categories (one per line)'
          FocusControl = memSupplementaryCats
          WordWrap = True
        end
        object Label14: TLabel
          Left = 6
          Top = 35
          Width = 107
          Height = 13
          Caption = 'Object type reference'
          FocusControl = edtObjectTypeRef
        end
        object Label20: TLabel
          Left = 6
          Top = 171
          Width = 113
          Height = 41
          AutoSize = False
          Caption = 'Subject references (one per line)'
          FocusControl = memSubjectRefs
          WordWrap = True
        end
        object Label15: TLabel
          Left = 6
          Top = 62
          Width = 127
          Height = 13
          Caption = 'Object attribute reference'
          FocusControl = edtObjectAttributeRef
        end
        object Label16: TLabel
          Left = 6
          Top = 327
          Width = 124
          Height = 41
          AutoSize = False
          Caption = 'Keywords or phrases (one per line)'
          FocusControl = memKeywords
          WordWrap = True
        end
        object Label24: TLabel
          Left = 6
          Top = 379
          Width = 92
          Height = 13
          Caption = 'Content location(s)'
          FocusControl = vleContentLocations
        end
        object Label25: TLabel
          Left = 6
          Top = 460
          Width = 63
          Height = 13
          Caption = 'Release date'
          FocusControl = dtpReleaseDate
        end
        object Label26: TLabel
          Left = 6
          Top = 487
          Width = 73
          Height = 13
          Caption = 'Expiration date'
          FocusControl = dtpExpirationDate
        end
        object Label27: TLabel
          Left = 6
          Top = 513
          Width = 91
          Height = 13
          Caption = 'Special instructions'
          FocusControl = edtSpecialInstructions
        end
        object Label28: TLabel
          Left = 6
          Top = 540
          Width = 70
          Height = 13
          Caption = 'Action advised'
          FocusControl = cboActionAdvised
        end
        object Label29: TLabel
          Left = 6
          Top = 569
          Width = 63
          Height = 13
          Caption = 'Date created'
          FocusControl = dtpDateCreated
        end
        object Label30: TLabel
          Left = 6
          Top = 595
          Width = 96
          Height = 13
          Caption = 'Digital creation date'
          FocusControl = dtpDigitalCreationDate
        end
        object Label31: TLabel
          Left = 6
          Top = 621
          Width = 95
          Height = 13
          Caption = 'Originating program'
          FocusControl = edtOriginatingProgram
        end
        object Label32: TLabel
          Left = 6
          Top = 648
          Width = 78
          Height = 13
          Caption = 'Program version'
          FocusControl = edtProgramVersion
        end
        object Label33: TLabel
          Left = 6
          Top = 675
          Width = 85
          Height = 13
          Caption = 'Object cycle code'
          FocusControl = edtObjectCycleCode
        end
        object Label34: TLabel
          Left = 6
          Top = 704
          Width = 41
          Height = 13
          Caption = 'Byline(s)'
          FocusControl = vleBylines
        end
        object Label35: TLabel
          Left = 6
          Top = 784
          Width = 19
          Height = 13
          Caption = 'City'
          FocusControl = edtCity
        end
        object Label36: TLabel
          Left = 6
          Top = 811
          Width = 59
          Height = 13
          Caption = 'Sub-location'
          FocusControl = edtSubLocation
        end
        object Label37: TLabel
          Left = 6
          Top = 838
          Width = 70
          Height = 13
          Caption = 'Province/state'
          FocusControl = edtProvinceOrState
        end
        object Label38: TLabel
          Left = 6
          Top = 865
          Width = 65
          Height = 13
          Caption = 'Country code'
          FocusControl = edtCountryCode
        end
        object Label39: TLabel
          Left = 6
          Top = 892
          Width = 68
          Height = 13
          Caption = 'Country name'
          FocusControl = edtCountryName
        end
        object Label40: TLabel
          Left = 6
          Top = 919
          Width = 119
          Height = 13
          Caption = 'Original transmission ref.'
          FocusControl = edtOriginalTransmissionRef
        end
        object Label41: TLabel
          Left = 6
          Top = 946
          Width = 41
          Height = 13
          Caption = 'Headline'
          FocusControl = edtHeadline
        end
        object Label42: TLabel
          Left = 6
          Top = 973
          Width = 29
          Height = 13
          Caption = 'Credit'
          FocusControl = edtCredit
        end
        object Label43: TLabel
          Left = 6
          Top = 1000
          Width = 33
          Height = 13
          Caption = 'Source'
          FocusControl = edtSource
        end
        object Label44: TLabel
          Left = 6
          Top = 1027
          Width = 79
          Height = 13
          Caption = 'Copyright notice'
          FocusControl = edtCopyrightNotice
        end
        object Label45: TLabel
          Left = 6
          Top = 1055
          Width = 110
          Height = 13
          Caption = 'Contacts (one per line)'
          FocusControl = memContacts
        end
        object Label46: TLabel
          Left = 6
          Top = 1106
          Width = 93
          Height = 13
          Caption = 'Caption or abstract'
          FocusControl = memCaptionOrAbstract
        end
        object Label47: TLabel
          Left = 6
          Top = 1157
          Width = 85
          Height = 38
          AutoSize = False
          Caption = 'Writers/editors (one per line)'
          FocusControl = memWritersOrEditors
          WordWrap = True
        end
        object Label48: TLabel
          Left = 6
          Top = 1207
          Width = 127
          Height = 24
          AutoSize = False
          Caption = 'Image orientation'
          FocusControl = cboImageOrientation
        end
        object edtFixtureIdentifier: TEdit
          Left = 148
          Top = 296
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 9
          OnChange = ControlChange
        end
        object edtObjectTypeRef: TEdit
          Left = 148
          Top = 32
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = ControlChange
        end
        object edtObjectAttributeRef: TEdit
          Left = 148
          Top = 59
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          OnChange = ControlChange
        end
        object edtCategoryCode: TEdit
          Left = 148
          Top = 218
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 7
          OnChange = ControlChange
        end
        object edtRecordVersion: TEdit
          Left = 148
          Top = 5
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = ControlChange
        end
        object edtObjectName: TEdit
          Left = 148
          Top = 86
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          OnChange = ControlChange
        end
        object memSubjectRefs: TMemo
          Left = 148
          Top = 167
          Width = 131
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 6
          WordWrap = False
          OnChange = ControlChange
        end
        object edtEditStatus: TEdit
          Left = 148
          Top = 113
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          OnChange = ControlChange
        end
        object cboUrgency: TComboBox
          Left = 148
          Top = 140
          Width = 131
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 5
          OnChange = ControlChange
        end
        object memSupplementaryCats: TMemo
          Left = 148
          Top = 245
          Width = 131
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 8
          WordWrap = False
          OnChange = ControlChange
        end
        object memKeywords: TMemo
          Left = 148
          Top = 323
          Width = 131
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 10
          WordWrap = False
          OnChange = ControlChange
        end
        object vleContentLocations: TValueListEditor
          Left = 148
          Top = 374
          Width = 131
          Height = 76
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
          Left = 148
          Top = 456
          Width = 131
          Height = 21
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 12
          OnChange = ControlChange
        end
        object dtpExpirationDate: TDateTimePicker
          Left = 148
          Top = 483
          Width = 131
          Height = 21
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 13
          OnChange = ControlChange
        end
        object edtSpecialInstructions: TEdit
          Left = 148
          Top = 510
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 14
          OnChange = ControlChange
        end
        object dtpDateCreated: TDateTimePicker
          Left = 148
          Top = 564
          Width = 131
          Height = 21
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 16
          OnChange = ControlChange
        end
        object dtpDigitalCreationDate: TDateTimePicker
          Left = 148
          Top = 591
          Width = 131
          Height = 21
          Date = 40177.000000000000000000
          Time = 40177.000000000000000000
          ShowCheckbox = True
          TabOrder = 17
          OnChange = ControlChange
        end
        object edtOriginatingProgram: TEdit
          Left = 148
          Top = 618
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 18
          OnChange = ControlChange
        end
        object edtProgramVersion: TEdit
          Left = 148
          Top = 645
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 19
          OnChange = ControlChange
        end
        object edtObjectCycleCode: TEdit
          Left = 148
          Top = 672
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 20
          OnChange = ControlChange
        end
        object vleBylines: TValueListEditor
          Left = 148
          Top = 699
          Width = 131
          Height = 76
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
          Left = 148
          Top = 781
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 22
          OnChange = ControlChange
        end
        object edtSubLocation: TEdit
          Left = 148
          Top = 808
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 23
          OnChange = ControlChange
        end
        object edtProvinceOrState: TEdit
          Left = 148
          Top = 835
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 24
          OnChange = ControlChange
        end
        object edtCountryCode: TEdit
          Left = 148
          Top = 862
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 25
          OnChange = ControlChange
        end
        object edtCountryName: TEdit
          Left = 148
          Top = 889
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 26
          OnChange = ControlChange
        end
        object edtOriginalTransmissionRef: TEdit
          Left = 148
          Top = 916
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 27
          OnChange = ControlChange
        end
        object edtHeadline: TEdit
          Left = 148
          Top = 943
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 28
          OnChange = ControlChange
        end
        object edtCredit: TEdit
          Left = 148
          Top = 970
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 29
          OnChange = ControlChange
        end
        object edtSource: TEdit
          Left = 148
          Top = 997
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 30
          OnChange = ControlChange
        end
        object edtCopyrightNotice: TEdit
          Left = 148
          Top = 1024
          Width = 131
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 31
          OnChange = ControlChange
        end
        object memContacts: TMemo
          Left = 148
          Top = 1051
          Width = 131
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 32
          WordWrap = False
          OnChange = ControlChange
        end
        object memCaptionOrAbstract: TMemo
          Left = 148
          Top = 1102
          Width = 131
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 33
          OnChange = ControlChange
        end
        object memWritersOrEditors: TMemo
          Left = 148
          Top = 1153
          Width = 131
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 34
          WordWrap = False
          OnChange = ControlChange
        end
        object cboActionAdvised: TComboBox
          Left = 148
          Top = 537
          Width = 131
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
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
          Left = 148
          Top = 1204
          Width = 131
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
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
    Left = 302
    Top = 8
    Width = 85
    Height = 360
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnOpen: TButton
      Left = 6
      Top = 16
      Width = 75
      Height = 25
      Action = actOpen
      TabOrder = 0
    end
    object btnSave: TButton
      Left = 6
      Top = 47
      Width = 75
      Height = 25
      Action = actSaveOrReload
      TabOrder = 1
    end
    object btnClose: TButton
      Left = 6
      Top = 155
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 4
      OnClick = btnCloseClick
    end
    object btnClear: TButton
      Left = 6
      Top = 86
      Width = 75
      Height = 25
      Caption = '&Clear'
      Enabled = False
      TabOrder = 2
      OnClick = btnClearClick
    end
    object btnReload: TButton
      Left = 6
      Top = 117
      Width = 75
      Height = 25
      Action = actSaveOrReload
      Caption = '&Reload File'
      TabOrder = 3
    end
  end
  object panFooter: TPanel
    Left = 8
    Top = 368
    Width = 379
    Height = 21
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4868682
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Padding.Left = 4
    Padding.Top = 6
    Padding.Right = 4
    ParentFont = False
    TabOrder = 2
    Visible = False
    object lblTagsWith: TLabel
      Left = 4
      Top = 6
      Width = 49
      Height = 15
      Align = alLeft
      Caption = 'Tags with '
      ExplicitHeight = 13
    end
    object lblHighlighted: TLabel
      Left = 53
      Top = 6
      Width = 52
      Height = 15
      Align = alLeft
      Caption = 'highlighted'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitHeight = 13
    end
    object lblLabelsHaveEtc: TLabel
      Left = 105
      Top = 6
      Width = 161
      Height = 15
      Align = alLeft
      Caption = ' labels have data in the saved file'
      ExplicitHeight = 13
    end
    object lblNoTagsFound: TLabel
      Left = 4
      Top = 6
      Width = 290
      Height = 15
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
    Filter = 'JPEG Image File (*.jpg,*.jpeg)|*.jpg;*.jpeg'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 600
    Top = 56
  end
end

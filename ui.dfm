object uiform: Tuiform
  Left = 360
  Top = 173
  Width = 523
  Height = 351
  Caption = 'Physiological Artifact Correction Tool'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 507
    Height = 29
    Caption = 'ToolBar1'
    TabOrder = 0
    object InspectBtn: TButton
      Left = 0
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Inspect'
      TabOrder = 1
      OnClick = InspectBtnClick
    end
    object PartBtn: TButton
      Left = 75
      Top = 2
      Width = 75
      Height = 22
      Caption = 'PART'
      TabOrder = 2
      OnClick = PartBtnClick
    end
    object ttestBtn: TButton
      Left = 150
      Top = 2
      Width = 75
      Height = 22
      Caption = 't-test'
      TabOrder = 3
      OnClick = ttestBtnClick
    end
    object AddBtn: TButton
      Left = 225
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Add'
      TabOrder = 0
      Visible = False
      OnClick = AddBtnClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 29
    Width = 507
    Height = 284
    Align = alClient
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Filter = 'NIFTI|*.nii;*.hdr;*.nii.gz'
    Left = 16
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.nii'
    Filter = 
      'NIFTI nii|*.nii|NIFTI compressed nii|*.nii.gz|NIFTI hdr+img|*.hd' +
      'r'
    Left = 48
    Top = 40
  end
  object OpenDialog2: TOpenDialog
    Filter = 'Siemens Physio|*.resp'
    Left = 80
    Top = 40
  end
end

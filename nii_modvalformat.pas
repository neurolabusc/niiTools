unit nii_modvalformat;

{$mode objfpc}{$H+}

interface
{$include isgui.inc}
uses
  {$ifdef GUI}Dialogs, {$endif}
  Classes, SysUtils, Dialogsx, define_types, valformat, nii_core;


procedure AddLesionVolumeToValFile( lFilename: string);

implementation

(*function SaveValCore (var lVALFilename:string; var lnSubj, lnFactors: integer; var lSymptomRA: singleP; var lImageNames:  TStrings; var lCrit,lCritPct: integer; var lPredictorList: TStringList):boolean;
const
	 kNative = 1;
	 kTxt = 2;
var
  f: TextFile;
  lFormat,C, R,lLen,lPos,ColStart,ColEnd,RowStart,RowEnd : integer ;
  lLevelStr,lFilename,S,lCell,lExt : string ;
  kSpacer,lDecimalSep : char;
  lError: boolean;
begin
	  lFormat := kNative;
   lExt := kValNativeExt;

   //lExt := StrUpper(PChar(extractfileext(SaveDialog1.Filename)));
   lFilename := changefileext(lVALFilename,lExt);
   lDecimalSep := DecimalSeparator;
         DecimalSeparator := '.';
   ChangeFileExt(lFilename,lExt);
	// Setup...
	kSpacer := #9; //tab
	S := '' ;
	RowStart := kMaxFactors+1 ;
	RowEnd := DataGrid.RowCount - 1;
	ColStart := 0 ;
	ColEnd := DataGrid.ColCount - 1;
	if (ColEnd < ColStart) or (RowEnd < RowStart) then exit;
	// Copy to string
	for R := RowStart to RowEnd do begin
		for C := ColStart to ColEnd do begin

                        lCell := DataGrid.Cells[ C, R ];
                        if C <> ColStart then begin
                           if lCell = '' then  //this simply prevents error reports when run from debugger
                              lError := true
                           else
                               Str2Float (lCell, lError);
                           if (lError) then
                              lCell := '-';
                        end;
			S := S +  lCell;
			if( C < DataGrid.ColCount - 1 ) then
				S := S + kSpacer{#9} ; // Tab
		end ;
		if R <> (DataGrid.RowCount - 1) then //all except last line
		   S := S + #13#10 ; // End line
	end ;
	AssignFile(f, lFileName);
	rewrite(f);
	if lFormat = kNative then begin
	   Self.Caption := kRegressSWVers+': '+extractfilename(SaveDialog1.Filename);//remove any previous filename
           if Files4D(DataGrid.Cells[ ColStart, RowStart ]) then
	      writeln(f,kVALNativeSignatureBase + '1')//version 1 supports 4D images
           else
	       writeln(f,kVALNativeSignatureBase + '0');//version 0 supports 3D images only

	   //Details for 1st factor
	   //writeln(f,'#Predictors:'+inttostr(lLen)+lLevelStr+lWithinSubjStr);
	   writeln(f,'#Covary Volume'+kSpacer+bool2char(DesignForm.LesionCovaryCheck.Checked));
	   writeln(f,'#Template'+kSpacer+DesignForm.TemplateLabel.Caption);
	   writeln(f,'#CritPct'+kSpacer+inttostr(DesignForm.CritPctEdit.value));
	   lLevelStr := 'ImageName';
	   lLen := DesignForm.AVal.value;
	   if lLen >= 1 then
	   	  for lPos := 1 to lLen do
	   		  lLevelStr := lLevelStr+kTab+(DesignForm.ALevelNames.Cells[lPos-1,0]);
	   writeln(f,lLevelStr);
	   gVALChanges := false;
	end;
	Writeln(f, S);
	Flush(f);  { ensures that the text was actually written to file }
	CloseFile(f);
    NoCancel := true;
    DecimalSeparator :=lDecimalSep;
end;  *)

function SaveValCore (var lVALFilename:string; var lnSubj, lnFactors: integer; var lSymptomRA: singleP; var lImageNames:  TStrings; var lCrit,lCritPct: integer; var lPredictorList: TStringList):boolean;
const
	 kNative = 1;
	 kTxt = 2;
var
  f: TextFile;
  lFormat,C, R,lLen,lPos: integer ;
  lLevelStr,lFilename,S,lCell,lExt : string ;
  kSpacer,lDecimalSep : char;
  lError: boolean;
begin
	  lFormat := kNative;
   lExt := kValNativeExt;

   //lExt := StrUpper(PChar(extractfileext(SaveDialog1.Filename)));
   lFilename := changefileext(lVALFilename,lExt);
   lDecimalSep := DecimalSeparator;
         DecimalSeparator := '.';
   ChangeFileExt(lFilename,lExt);
	// Setup...
	kSpacer := #9; //tab
	S := '' ;

	if (lnFactors < 1) or (lnSubj < 1) then exit;
	// Copy to string
         (*for lCol := 1 to lnFactors do begin
            for lRow := 1 to lnSubj do begin
              lSymptomRA^[lRow+ ((lCol-1)*lnSubj)] := lInRA^[(lRow*lnColWObs)-lnColWObs-1+lCol];
            end;
        end;*)
    for R := 1 to lnSubj do begin
        S := S + lImageNames[R-1]+kSpacer;
    	for C := 1 to lnFactors do begin
            lCell := Floattostr(lSymptomRA^[R+((C-1)*lnSubj)]);
			S := S +  lCell;
			if( C < lnFactors ) then
				S := S + kSpacer{#9} ; // Tab
		end ;
		if R <> (lnSubj) then //all except last line
		   S := S + #13#10 ; // End line
	end ;
	AssignFile(f, lFileName);
	rewrite(f);
	if lFormat = kNative then begin
	       writeln(f,kVALNativeSignatureBase + '0');//version 0 supports 3D images only

	   //Details for 1st factor
	   //writeln(f,'#Predictors:'+inttostr(lLen)+lLevelStr+lWithinSubjStr);
	   writeln(f,'#Covary Volume'+kSpacer+bool2char(true));
	   writeln(f,'#Template'+kSpacer+'');
	   writeln(f,'#CritPct'+kSpacer+inttostr(lCritPct));
	   lLevelStr := 'ImageName';
	   lLen := lPredictorList.count;
	   if lLen >= 1 then
	   	  for lPos := 1 to lLen do
	   		  lLevelStr := lLevelStr+kTab+(lPredictorList[lPos-1]);
	   writeln(f,lLevelStr);
	end;
	Writeln(f, S);
	Flush(f);  { ensures that the text was actually written to file }
	CloseFile(f);
    DecimalSeparator :=lDecimalSep;
end;

procedure AddLesionVolumeToValFile( lFilename: string);
label
  	 666;
var
  lForceVOI: boolean;
  lVALFilename,lLesionName: string ;
  lnSubj, lnFactors,lnFactorsx: integer;
  lVol: LongintP;
  lSymptomRA,lSymptomRAx: singleP;
  lImageNames:  TStrings;
  lCrit,lCritPct: integer;
  lPredictorList: TStringList;
  lS,lv,lnV,lCol,lRow: integer;
  lNII: TNIFTIImg;
begin
  lImageNames := TStringList.Create;
  lVALFilename := lFilename;
  {$IFDEF GUI}
  if MessageDlg('Preferences', 'Retain image filename extension? (If no, all .nii files renamed renamed .voi)'  , mtConfirmation,[mbYes, mbNo],'0') = mrNo then
    lForceVOI := true
  else
  {$ENDIF}
      lForceVOI := false;


  if not GetValCore ( lVALFilename,lnSubj, lnFactors, lSymptomRA, lImageNames, lCrit,lCritPct, lPredictorList) then
  	 exit;
  if (lImageNames.Count <> lnSubj) or (lnSubj < 1) then
  	 exit;
   getmem(lvol,lnSubj * sizeof(longint) );
  CreateNii(lNII);
  for lS := 1 to lnSubj do begin
         if lForceVOI then
                      lLesionName := ChangeFileExtX(lImageNames[lS-1],'.voi')
         else
             lLesionName := ChangeFileExtX(lImageNames[lS-1],'.nii');
         if not fileexists(lLesionName) then  begin
           showmsg('Unable to find lesion map '+lLesionName);
           goto 666;
         end;
         if not Read3DBinary(lLesionName, lNII) then begin
               showmsg('Error loading lesion'+lLesionName);
               goto 666;
         end;
         lnV := lNII.VoxelsLoaded;
         lVol^[lS] := 0;
         for lV := 1 to lnV do
             if  (lNII.i8^[lV] <> 0) then
                inc(lVol^[lS]);

         riteln(lLesionName+kTab+inttostr(lVol^[lS]) );
         lImageNames[lS-1] := extractfilename(lImageNames[lS-1]);
  end;
  lPredictorList.Add('LesionVolume');
  lnFactorsx := lnFactors+1;
  getmem(lSymptomRAx,lnSubj* lnFactorsx* sizeof(single));
  for lCol := 1 to lnFactors do
  	  for lRow := 1 to lnSubj do
              lSymptomRAx^[lRow+ ((lCol-1)*lnSubj)] :=  lSymptomRA^[lRow+ ((lCol-1)*lnSubj)];
  lCol := lnFactorsX;
  for lRow := 1 to lnSubj do
  	  lSymptomRAx^[lRow+ ((lCol-1)*lnSubj)] := lVol^[lRow];




  lVALFilename := ChangeFilePostFix(lVALFilename,'vol');

  SaveValCore ( lVALFilename, lnSubj, lnFactorsx, lSymptomRAx, lImageNames, lCrit,lCritPct, lPredictorList);
  666:
  freeNII(lNII);
  freemem(lvol);
  lPredictorList.free;
  lImageNames.free;
  freemem(lSymptomRA);
  freemem(lSymptomRAx);

end;

end.

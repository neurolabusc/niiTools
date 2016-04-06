unit nii_mask;

{$mode objfpc}{$H+}

interface
uses Classes, define_types, nii_core, sysutils, dialogsx;

function nii_masking (var lMask,lNII:TNIfTIimg; Thresh: single):boolean; overload;
function nii_masking (lMask: string; lImages: Tstrings; Thresh: single):boolean; overload;

implementation

function nii_masking (var lMask,lNII:TNIfTIimg; Thresh, Fill: single; MaskVol: integer):boolean; overload;
//All voxels that are < Thresh in the Mask are set to Fill
//MaskVol: if mask is a 4D image, then use the Nth volume as the mask, e.g. SPM NewSeg has a 4D TPM

var
  lS: single;
  lNIIOffset,lMaskOffset,lVox,lnVox,lVol,lnVol: integer;
  lImg: singlep;
begin
  result := false;
  if not SameXYZDims(lMask,lNII) then
     exit;
  lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3];
  lnVol := lNII.hdr.dim[4];
  if lnVol < 1 then lnVol := 1;
  if (lnVol < 1) or (lnVox < 1) or (lNII.hdr.datatype <> kDT_FLOAT)  then begin
        riteln('nii_mask error: only available for 32-bit single precision data with at least 3D.');
        exit;
  end;
  if (MaskVol <= 1) or (MaskVol > lMask.hdr.dim[4]) then
     lMaskOffset := 0
  else
      lMaskOffset := (MaskVol-1) *lnVox;
  for lVol := 1 to lnVol do begin
      lNIIOffset := (lVol-1) *lnVox;
      for lVox:= 1 to (lnVol*lnVox) do begin
          lS := lMask.f32^[lMaskOffset+lVox];
          if specialsingle(lS) or (lS <= Thresh) then
             lNII.f32^[lNIIOffset+lVox] := Fill;
      end; //each Vox

  end; //each vol
  result := true;
end;//niiMask

function nii_masking (var lMask,lNII:TNIfTIimg; Thresh: single):boolean; overload;
begin
  result := nii_masking ( lMask,lNII,Thresh,0,1);

end;

function FirstDigit(Name: string): integer;
//returns first digit in name
//Determine tissue map number
//e.g. "wc2subj1" and "c2subj1" both return 2
//assumes input is extracted filename, e.g. "c:\folder7\c2subj1" will return 7
var
  len,i: integer;
begin
     result := -1;//error
     len := length(Name);
     if len < 1 then
        exit;
     for i := 1 to len do begin
         if Name[i] in ['0'..'9'] then begin
               result := strtoint(Name[i]);
               exit;
         end;
     end;//for each character in Name

end;

function nii_mask4D (lMask,lNII: TNIfTIimg; lImages: Tstrings; Thresh: single):boolean; overload;
//lMask is a 4D image, e.g. SPM TPM, apply to c1,c2,c3... normalized data...
var
  lI,lVol,lNImg: integer;
begin
     result := false;
     lNImg := lImages.Count;
     if lNImg < 1 then begin
        riteln('nii_mask error: no files');
      exit;
     end;
     for lI := 1 to lNImg do begin
         lVol := FirstDigit(extractfilename(lImages[lI-1]));
         if not Read4DF32(lImages[lI-1], lNII) then  begin
            showmsg('Error loading '+lImages[lI-1]);
         end else if not SameXYZDims(lMask,lNII) then begin
            showmsg('Image and mask must have the same dimensions '+lImages[lI-1]);
         end else if (lVol < 1) or (lVol > lMask.hdr.dim[4]) then begin
            showmsg('4D masking volume has '+inttostr(lMask.hdr.dim[4])+' volumes, assumption that first digit in filename is 1..'+inttostr(lMask.hdr.dim[4])+' is not met: '+extractfilename(lImages[lI-1]));
         end else begin

             lNII.HdrName := changefileprefix(lImages[lI-1],'m');//masked
             result := nii_masking(lMask,lNII,Thresh,0,lVol);
             if result then begin
             	WriteNII(lNII.HdrName,lNII);
          	riteln('masked '+lNII.HdrName+' with volume'+inttostr(lVol)+' of '+lMask.HdrName);
             end;

         end;
     end;

end;

function nii_masking (lMask: string; lImages: Tstrings; Thresh: single):boolean; overload;
label
  666;
var
  lNImg,lI: integer;
  lMaskImg,lNIIImg: TNIfTIimg;
begin
     result := false;
     lNImg := lImages.Count;
     if lNImg < 1 then begin
        riteln('nii_mask error: no files');
      exit;
     end;
     CreateNII(lNIIImg);
     CreateNII(lMaskImg);
     if not Read4DF32(lMask, lMaskImg) then begin
        showmsg('Error loading mask '+lMask);
        goto 666;
     end;
     if (lMaskImg.hdr.dim[4] > 1) then begin
        result := nii_mask4D(lMaskImg,lNIIImg, lImages,Thresh);
        goto 666;
     end;
     for lI := 1 to lNImg do begin
         if not Read4DF32(lImages[lI-1], lNIIImg) then  begin
            showmsg('Error loading '+lImages[lI-1]);
            goto 666;
         end;
         if not SameXYZDims(lMaskImg,lNIIImg) then begin
            showmsg('Image and mask must have the same dimensions '+lImages[lI-1]);
         end else begin
             lNIIImg.HdrName := changefileprefix(lImages[lI-1],'m');//masked
             result := nii_masking(lMaskImg,lNIIImg,Thresh);
             if result then begin
             	WriteNII(lNIIImg.HdrName,lNIIImg);
          	riteln('masked '+lNIIImg.HdrName+' with '+lMask);
             end;

         end;
     end;
666:
     FreeNII(lMaskImg);
     FreeNII(lNIIImg);
end;


end.


unit ezdicom;

interface
uses define_types, sysutils,dialogsx;
(*Simple routines for reading specific DICOM tags.
 Only works with well-behaved DICOM images...
  showmessage(inttostr(DICOMmsSinceMidNight'C:\f1\00001.dcm')));
  showmessage(DICOMgettag($0008,$0032,'C:\f1\00001.dcm'));
*)
function DICOMgettag(lGroup,lElement: integer;  lFileName: string): string;
function DICOMmsSinceMidNight(lFileName: string): integer;
function msSinceMidNight (lStr: string): integer;


implementation

function DICOMgettag(lGroup,lElement: integer;  lFileName: string): string;
label
999;
const
     kMaxBuf = (256*256)-1; //bytes
     kMax16bit = (256*256)-1;
     kImageStart = $7FE0+($0010 shl 16 );
     kMaxFloats = 6;
var
  lBufferSz,lPos,lFileSz,lBuffStart,lOffset, little_endian,
   lDesiredGroupElement,lTemp,lGroupElement,lLength: integer;
  vr : array [1..2] of Char;
  lFailure: boolean;
  lByteRA: Bytep;
  lInFile: file;
function GetByte (lFilePos: integer): byte;
var
   lBufPos: integer;
begin
    //the following error checking slows down reads a lot!
    //a simpler alternative would be to make the buffer size the same size as the entire image...
    //the current strategy saves memory and is faster for large images with small headers
    result := 0;
    if lFilepos > lFileSz then begin
        lFailure := true;

        exit;
    end;
    lBufPos := lFilepos - lBuffStart+1;
    if (lBufPos > lBufferSz) or (lBufPos < 1) then begin //reload buffer
       if (lFilePos < 0) or (lFilePos > lFileSz) then begin
          lBuffStart := lFilePos;
          msg(lFileName);
          lFailure := true;
          Msg('Error: buffer overrun in DICOM read.');
          exit;
       end;
       if lFilePos+kMaxBuf > lFileSz then
          lBufferSz := lFileSz - (lFilePos)
       else
           lBufferSz := kMaxBuf; //read remaining
       AssignFile(lInFile, lFileName);
       FileMode := 0;  //Set file access to read only
       Reset(lInFile, 1);
       seek(lInFile,lFilePos);
       BlockRead(lInFile, lByteRA^[1], lBufferSz);
       CloseFile(lInFile);
       FileMode := 2;
       lBuffStart := lFilePos;
       lBufPos := 1;
    end;
    result := lByteRA^[lBufPos];
end; //nested GetByte
function ReadInt4: integer; 
begin
    if little_endian = 0 then
        result := GetByte(lPos+3)+(GetByte(lPos+2) shl 8)+(GetByte(lPos+1) shl 16)+(GetByte(lPos) shl 24)
    else
        result := GetByte(lPos)+(GetByte(lPos+1) shl 8)+(GetByte(lPos+2) shl 16)+(GetByte(lPos+3) shl 24);
    inc(lPos,4);
end; //nested function Read4
procedure ReadGroupElementLength(var lGroupElement,lLength: integer);
begin
     lGroupElement := ReadInt4;
     vr[1] := chr(GetByte(lPos));
     vr[2] := chr(GetByte(lPos+1));
     if (vr[2] < 'A')  then begin //implicit vr with 32-bit length
        lLength := ReadInt4;
        exit;
     end;
     if (vr = 'OB') or (vr = 'OW') or (vr = 'SQ') or (vr = 'UN') then begin  {explicit VR with 32-bit length}
          lPos := lPos + 4;  {skip 2 byte string and 2 reserved bytes = 4 bytes = 2 words}
          lLength := ReadInt4;//Ord4(buf[lPos]) + $100 * (buf[lPos+1] + $100 * (buf[lPos+2] + $100 * buf[lPos+3]))
     end else begin {explicit VR with 16-bit length}
         if little_endian = 0  then
             lLength := (GetByte(lPos+3))+(GetByte(lPos+2) shl 8)
         else
             lLength := (GetByte(lPos+2))+(GetByte(lPos+3) shl 8);//GetLength := Ord4(buf[i+2]) + $100 * (buf[i+3]);
         lPos := lPos + 4;  {skip 2 byte string and 2 length bytes = 4 bytes = 2 words}
     end;
end; //nested procedure ReadGroupElementLength
function DCMStr(lBytes: integer): string;
var
   lC: integer;
begin
    result := '';
    if lBytes < 1 then
       exit;
    for lC := lPos to (lPos+(lBytes-1)) do
        result := result + char(GetByte(lC));
    for lC := 1 to lBytes do
        if result[lC]  in ['+','-','/','\',' ','0'..'9','a'..'z','A'..'Z','.'] then
        else
           result[lC] := ' ';
end; //nested function DCMStr
begin //main function
   lFailure := false;
   lDesiredGroupElement := lGroup+(lElement shl 16 );
     little_endian := 1;
     lOffset := 128;
     result := '';
     lFileSz := FSize(lFilename);
     lBufferSz := lFileSz-lOffset;
     if lBufferSz < 512 then begin
        //showmessage('Error: File too small '+lFilename);
        exit;
     end;
     if lBufferSz > kMaxBuf then
        lBufferSz := kMaxBuf;
     GetMem(lByteRA,kMaxBuf);
     lBufferSz := lBufferSz;
     AssignFile(lInFile, lFileName);
     FileMode := 0;  //Set file access to read only
     Reset(lInFile, 1);
     seek(lInFile,lOffset);
     BlockRead(lInFile, lByteRA^[1], lBufferSz);
     CloseFile(lInFile);
     FileMode := 2;
     lBuffStart := lOffset;
     lPos := lOffset;
     if lOffset = 128 then begin //DICOM files start with DICM at 128, Siemens shadow headers do not
        if DCMStr(4) <> 'DICM' then begin
           Msg(DCMStr(4)+ ' <> DICM');
           FreeMem(lByteRA);
           exit;
        end;
        lPos := lOffset + 4;//DICM read
     end;//Offset = 128
     if not( chr(GetByte(lPos+4)) in ['A'..'Z']) or not( chr(GetByte(lPos+5)) in ['A'..'Z']) then
        Msg('implicit VR untested');
     //next check Endian
     lTemp := lPos;
     ReadGroupElementLength(lGroupElement,lLength);
     if lLength > kMax16bit then
        Msg('ByteSwapped');
     lPos := lTemp;
     //end VR check
     while  (not lFailure) do begin
         ReadGroupElementLength(lGroupElement,lLength);
         if (not lFailure) and (lGroupElement =lDesiredGroupElement) then begin
                     result := (DCMStr(lLength));
                     goto 999;
                 end;
         lPos := lPos + (lLength);
     end; //while imagestart=0 and not error
   999:
     FreeMem(lByteRA);
end; //function fast_read_dicom_data

function msSinceMidNight (lStr: string): integer; //convert 'HHMMSS.SSS' string to ms elapsed since midnight
var
  lNumStr: string;
  ms,i,len,dec: integer;
begin
  result := -1;//error

  if lStr = '' then
    exit;
  len := length(lStr);
  lNumStr := '';
  for i := 1 to len do begin
    if (lStr[i] = '.') or (lStr[i] = ',') then
      lStr[i] := DecimalSeparator; //make native format, e.g. in Germany 10,123 whereas in USA 10.123
    if lStr[i] in ['0'..'9',DecimalSeparator] then
      lNumStr := lNumStr + lStr[i];
  end;
  if lNumStr = '' then
    exit;
  //make sure 6 characters before decimal, in case HHMMSS is written HMMSS
  dec := length(lNumStr) + 1;
  for i := length(lNumStr) downto 1 do
    if lNumStr[i] = DecimalSeparator then
      dec := i;
  if dec > 7 then
    exit; //HHMMSS.??? can only have 6 digits before decimal
  while dec < 7 do begin
    lNumStr := '0'+lNumStr;
    inc(dec);
  end;
  //now in HHMMSS.????? format
  len := length(lNumStr);
  lStr := lNumStr[1]+lNumStr[2]; //HH
  ms := 1000 * 60 * 60 * strtoint(lStr); //60m/h, 60s/m 1000ms/s
  lStr := lNumStr[3]+lNumStr[4]; //MM
  ms := ms + (1000 * 60 * strtoint(lStr)); //60s/m 1000ms/s
  lStr := '';
  for i := 5 to len do //SS.SSSS
    lStr := lStr + lNumStr[i];
  ms := ms + round(1000 * strtofloat(lStr));//1000 ms/s
  result := ms;

end;

function DICOMmsSinceMidNight(lFileName: string): integer;
//DICOM $0008:$0032 stores HHMMSS.SSSS since midnight convert to ms since midnight
// not sure if H is always two digits, so reading a bit tricky...
begin
  result := msSinceMidNight(DICOMgettag($0008,$0032,lFileName));
end;

end.

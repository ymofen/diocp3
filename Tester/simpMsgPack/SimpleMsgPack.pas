unit SimpleMsgPack;

interface

uses
  classes, SysUtils;

type
  {$IF RTLVersion<25}
    IntPtr=Integer;
  {$IFEND IntPtr}


  TMsgPackType = (mptUnknown, mptMap, mptString, mptInteger, mptBoolean, mptFloat, mptBinary);

  IMsgPack = interface
    ['{37D3E479-7A46-435A-914D-08FBDA75B50E}'] 
  end;

  TMsgPackSetting = class(TObject)
  private
    FCaseSensitive: Boolean;
  public
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

  TSimpleMsgPack = class(TObject)
  private
    FParent:TSimpleMsgPack;



    FName:String;

    FValue:TBytes;

    FDataType:TMsgPackType;

    FChildren: TList;

    function InnerAdd: TSimpleMsgPack;
    function GetCount: Integer;
    procedure InnerEncodeToStream(pvStream:TStream);
    procedure InnerParseFromStream(pvStream: TStream);
  private
    function getAsString: String;
    procedure setAsString(pvValue:string);

    function getAsInteger: Int64;
    procedure setAsInteger(pvValue:Int64);

    procedure checkObjectDataType(ANewType: TMsgPackType = mptMap);
    function GetO(pvPath: String): TSimpleMsgPack;
    procedure SetO(pvPath: String; const Value: TSimpleMsgPack);

    function findObj(pvName:string): TSimpleMsgPack;
    function indexOfCaseSensitive(pvName:string): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;



    procedure LoadBinaryFromStream(pvStream: TStream; pvLen: cardinal = 0);
    procedure SaveBinaryToStream(pvStream:TStream);

    procedure EncodeToStream(pvStream:TStream);
    procedure DecodeFromStream(pvStream:TStream);

    function EncodeToBytes: TBytes;
    procedure DecodeFromBytes(pvBytes:TBytes);



    function Add(pvNameKey, pvValue: string): TSimpleMsgPack; overload;
    function Add(pvNameKey: string; pvValue: Int64): TSimpleMsgPack; overload;
    function Add(pvNameKey: string; pvValue: TBytes): TSimpleMsgPack; overload;
    function Add(pvNameKey: String): TSimpleMsgPack; overload;
    function Add():TSimpleMsgPack; overload;

    property AsInteger:Int64 read getAsInteger write setAsInteger;
    
    property AsString:string read getAsString write setAsString;


    property O[pvPath: String]: TSimpleMsgPack read GetO write SetO;
  end;

implementation



type
  // copy from qmsgPack
  TMsgPackValue=packed record
    ValueType:Byte;
    case Integer of
      0:(U8Val:Byte);
      1:(I8Val:Shortint);
      2:(U16Val:Word);
      3:(I16Val:Smallint);
      4:(U32Val:Cardinal);
      5:(I32Val:Integer);
      6:(U64Val:UInt64);
      7:(I64Val:Int64);
      8:(F32Val:Single);
      9:(F64Val:Double);
      10:(BArray:array[0..16] of Byte);
  end;


function swap16(const v): Word;
begin
  // FF, EE : EE->1, FF->2
  PByte(@result)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 1)^ := PByte(@v)^;
end;

function swap32(const v): Cardinal;
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@result)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 3)^ := PByte(@v)^;
end;

function swap64(const v): Int64;
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@result)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@result) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@result) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 7)^ := PByte(@v)^;
end;


function getFirst(var strPtr:PAnsiChar; splitChars:TSysCharSet):AnsiString;
var
  oPtr:PAnsiChar;
  l:Cardinal;
begin
  oPtr := strPtr;
  Result := '';
  while True do
  begin
    if (strPtr^ in splitChars) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
        SetLength(Result, l);
        Move(oPtr^, Result[1], l);
        break;
      end;
    end else if (strPtr^ = #0) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
        SetLength(Result, l);
        Move(oPtr^, Result[1], l);
      end;
      break;
    end;
    Inc(strPtr);
  end;
end;

// copy from qmsgPack
procedure writeString(pvValue: string; pvStream: TStream);
var
  lvRawStr:AnsiString;
  l:Integer;
  lvValue:TMsgPackValue;
begin
  lvRawStr:=Utf8Encode(pvValue);
  l:=Length(lvRawStr);

  //
  //fixstr stores a byte array whose length is upto 31 bytes:
  //+--------+========+
  //|101XXXXX|  data  |
  //+--------+========+
  //
  //str 8 stores a byte array whose length is upto (2^8)-1 bytes:
  //+--------+--------+========+
  //|  0xd9  |YYYYYYYY|  data  |
  //+--------+--------+========+
  //
  //str 16 stores a byte array whose length is upto (2^16)-1 bytes:
  //+--------+--------+--------+========+
  //|  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
  //+--------+--------+--------+========+
  //
  //str 32 stores a byte array whose length is upto (2^32)-1 bytes:
  //+--------+--------+--------+--------+--------+========+
  //|  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
  //+--------+--------+--------+--------+--------+========+
  //
  //where
  //* XXXXX is a 5-bit unsigned integer which represents N
  //* YYYYYYYY is a 8-bit unsigned integer which represents N
  //* ZZZZZZZZ_ZZZZZZZZ is a 16-bit big-endian unsigned integer which represents N
  //* AAAAAAAA_AAAAAAAA_AAAAAAAA_AAAAAAAA is a 32-bit big-endian unsigned integer which represents N
  //* N is the length of data

  if L<=31 then
  begin
    lvValue.ValueType:=$A0+Byte(L);
    pvStream.WriteBuffer(lvValue.ValueType,1);
  end
  else if L<=255 then
  begin
    lvValue.ValueType:=$d9;
    lvValue.U8Val:=Byte(L);
    pvStream.WriteBuffer(lvValue,2);
  end
  else if L<=65535 then
  begin
    lvValue.ValueType:=$da;
    lvValue.U16Val:=((L shr 8) and $FF) or ((L shl 8) and $FF00);
    pvStream.Write(lvValue,3);
  end else
  begin
    lvValue.ValueType:=$db;
    lvValue.BArray[0]:=(L shr 24) and $FF;
    lvValue.BArray[1]:=(L shr 16) and $FF;
    lvValue.BArray[2]:=(L shr 8) and $FF;
    lvValue.BArray[3]:=L and $FF;
    pvStream.WriteBuffer(lvValue,5);
  end;

  pvStream.Write(PByte(lvRawStr)^, l);
end;

// copy from qmsgPack
procedure WriteInt(const iVal: Int64; AStream: TStream);
var
  lvValue:TMsgPackValue;
begin
  if iVal>=0 then
    begin
    if iVal<=127 then
      begin
      lvValue.U8Val:=Byte(iVal);
      AStream.WriteBuffer(lvValue.U8Val,1);
      end
    else if iVal<=255 then//UInt8
      begin
      lvValue.ValueType:=$cc;
      lvValue.U8Val:=Byte(iVal);
      AStream.WriteBuffer(lvValue,2);
      end
    else if iVal<=65535 then
      begin
      lvValue.ValueType:=$cd;
      lvValue.BArray[0]:=(iVal shr 8);
      lvValue.BArray[1]:=(iVal and $FF);
      AStream.WriteBuffer(lvValue,3);
      end
    else if iVal<=Cardinal($FFFFFFFF) then
      begin
      lvValue.ValueType:=$ce;
      lvValue.BArray[0]:=(iVal shr 24) and $FF;
      lvValue.BArray[1]:=(iVal shr 16) and $FF;
      lvValue.BArray[2]:=(iVal shr 8) and $FF;
      lvValue.BArray[3]:=iVal and $FF;
      AStream.WriteBuffer(lvValue,5);
      end
    else
      begin
      lvValue.ValueType:=$cf;
      lvValue.BArray[0]:=(iVal shr 56) and $FF;
      lvValue.BArray[1]:=(iVal shr 48) and $FF;
      lvValue.BArray[2]:=(iVal shr 40) and $FF;
      lvValue.BArray[3]:=(iVal shr 32) and $FF;
      lvValue.BArray[4]:=(iVal shr 24) and $FF;
      lvValue.BArray[5]:=(iVal shr 16) and $FF;
      lvValue.BArray[6]:=(iVal shr 8) and $FF;
      lvValue.BArray[7]:=iVal and $FF;
      AStream.WriteBuffer(lvValue,9);
      end;
    end
  else//<0
    begin
    if iVal<=-2147483648 then//64λ
      begin
      lvValue.ValueType:=$d3;
      lvValue.BArray[0]:=(iVal shr 56) and $FF;
      lvValue.BArray[1]:=(iVal shr 48) and $FF;
      lvValue.BArray[2]:=(iVal shr 40) and $FF;
      lvValue.BArray[3]:=(iVal shr 32) and $FF;
      lvValue.BArray[4]:=(iVal shr 24) and $FF;
      lvValue.BArray[5]:=(iVal shr 16) and $FF;
      lvValue.BArray[6]:=(iVal shr 8) and $FF;
      lvValue.BArray[7]:=iVal and $FF;
      AStream.WriteBuffer(lvValue,9);
      end
    else if iVal<=-32768 then
      begin
      lvValue.ValueType:=$d2;
      lvValue.BArray[0]:=(iVal shr 24) and $FF;
      lvValue.BArray[1]:=(iVal shr 16) and $FF;
      lvValue.BArray[2]:=(iVal shr 8) and $FF;
      lvValue.BArray[3]:=iVal and $FF;
      AStream.WriteBuffer(lvValue,5);
      end
    else if iVal<=-128 then
      begin
      lvValue.ValueType:=$d1;
      lvValue.BArray[0]:=(iVal shr 8);
      lvValue.BArray[1]:=(iVal and $FF);
      AStream.WriteBuffer(lvValue,3);
      end
    else if iVal<-32 then
      begin
      lvValue.ValueType:=$d0;
      lvValue.I8Val:=iVal;
      AStream.WriteBuffer(lvValue,2);
      end
    else
      begin
      lvValue.I8Val:=iVal;
      AStream.Write(lvValue.I8Val,1);
      end;
    end;//End <0
end;


procedure writeMap(obj:TSimpleMsgPack; pvStream:TStream);
var
  c, i:Integer;
  lvValue:TMsgPackValue;
  lvNode:TSimpleMsgPack;
begin
  C:=obj.Count;
  if C<=15 then
  begin
    lvValue.ValueType:=$80+C;
    pvStream.WriteBuffer(lvValue.ValueType,1);
  end
  else if C<=65535 then
  begin
    lvValue.ValueType:=$de;
    lvValue.BArray[0]:=(C shr 8) and $FF;
    lvValue.BArray[1]:=C and $FF;
    pvStream.WriteBuffer(lvValue,3);
  end
  else
  begin
    lvValue.ValueType:=$df;
    lvValue.BArray[0]:=(C shr 24) and $FF;
    lvValue.BArray[1]:=(C shr 16) and $FF;
    lvValue.BArray[2]:=(C shr 8) and $FF;
    lvValue.BArray[3]:=C and $FF;
    pvStream.WriteBuffer(lvValue,5);
  end;
  for I := 0 to C-1 do
  begin
    lvNode:=obj.FChildren[I];
    writeString(lvNode.FName, pvStream);
    lvNode.InnerEncodeToStream(pvStream);
  end;
end;


constructor TSimpleMsgPack.Create;
begin
  inherited Create;
  FChildren := TList.Create();
end;

procedure TSimpleMsgPack.DecodeFromBytes(pvBytes: TBytes);
var
  lvStream:TStream;
begin
  lvStream := TMemoryStream.Create;
  try
    lvStream.Write(pvBytes[0], Length(pvBytes));
    lvStream.Position := 0;
    DecodeFromStream(lvStream);
  finally
    lvStream.Free;
  end;

end;

procedure TSimpleMsgPack.DecodeFromStream(pvStream: TStream);
begin
  InnerParseFromStream(pvStream);
end;

destructor TSimpleMsgPack.Destroy;
begin
  FChildren.Free;
  FChildren := nil;
  inherited Destroy;
end;

function TSimpleMsgPack.Add(pvNameKey, pvValue: string): TSimpleMsgPack;
begin
  Result := InnerAdd;
  Result.FName := pvNameKey;
  Result.AsString := pvValue;
end;

function TSimpleMsgPack.Add(pvNameKey: string; pvValue: Int64): TSimpleMsgPack;
begin
  Result := InnerAdd;
  Result.FName := pvNameKey;
  Result.AsInteger := pvValue;
end;


function TSimpleMsgPack.Add: TSimpleMsgPack;
begin
  Result := InnerAdd;
end;

function TSimpleMsgPack.Add(pvNameKey: string; pvValue: TBytes): TSimpleMsgPack;
begin
  Result := InnerAdd;
  Result.FName := pvNameKey;
  Result.FDataType := mptBinary;
  Result.FValue := pvValue;
end;

function TSimpleMsgPack.Add(pvNameKey:String): TSimpleMsgPack;
begin
  Result := InnerAdd;
  Result.FName := pvNameKey;
end;

procedure TSimpleMsgPack.checkObjectDataType(ANewType: TMsgPackType = mptMap);
begin
  if not (FDataType in [mptMap]) then
  begin
    FDataType := ANewType;
  end;
  
end;

function TSimpleMsgPack.EncodeToBytes: TBytes;
var
  lvStream:TStream;
begin
  lvStream := TMemoryStream.Create;
  try
    EncodeToStream(lvStream);
    lvStream.Position := 0;
    SetLength(Result, lvStream.size);
    lvStream.Read(Result[0], lvStream.Size);
  finally
    lvStream.Free;
  end;
end;

procedure TSimpleMsgPack.EncodeToStream(pvStream: TStream);
begin
  InnerEncodeToStream(pvStream);
end;

function TSimpleMsgPack.findObj(pvName:string): TSimpleMsgPack;
var
  i:Integer;
begin
  i := indexOfCaseSensitive(pvName);
  Result := TSimpleMsgPack(FChildren[i]);
end;

function TSimpleMsgPack.getAsInteger: Int64;
begin
  case FDataType of
    mptInteger: Result:=PInt64(FValue)^;
  else
    Result := 0;
  end;
end;

function TSimpleMsgPack.getAsString: String;
var
  l:Cardinal;
begin
  if FDataType = mptString then
  begin
    l := Length(FValue);
    if l = 0 then
    begin
      Result := '';
    end else if SizeOf(Char) = 2 then
    begin
      SetLength(Result, l shr 1);
      Move(FValue[0],PChar(Result)^, l);
    end else
    begin
      SetLength(Result, l);
      Move(FValue[0],PChar(Result)^, l);
    end;
  end else if FDataType = mptInteger then           
  begin
    Result := IntToStr(getAsInteger);
  end else
  begin
    Result := '';
  end;
end;

function TSimpleMsgPack.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TSimpleMsgPack.GetO(pvPath: String): TSimpleMsgPack;
var
  lvName:AnsiString;
  s:AnsiString;
  sPtr:PAnsiChar;
begin
  s := pvPath;

  sPtr := PAnsiChar(s);
  lvName := getFirst(sPtr, ['.', '/','\']);
  if lvName = '' then
  begin
    Result := nil;
  end else
  begin

  end;


  Result := nil;
end;

function TSimpleMsgPack.indexOfCaseSensitive(pvName:string): Integer;
var
  i, l: Integer;
  lvObj:TSimpleMsgPack;
begin
  Result := -1;
  l := Length(pvName);
  if l = 0 then exit;
  for i := 0 to FChildren.Count-1 do
  begin
    lvObj := TSimpleMsgPack(FChildren[i]);
    if Length(lvObj.FName) = l then
    begin
      if lvObj.FName = pvName then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

function TSimpleMsgPack.InnerAdd: TSimpleMsgPack;
begin
  checkObjectDataType(mptMap);
  Result := TSimpleMsgPack.Create;
  Result.FParent := self;
  Result.FDataType := mptUnknown;
  FChildren.Add(Result);
end;

procedure TSimpleMsgPack.InnerEncodeToStream(pvStream:TStream);
begin
  case FDataType of
    mptUnknown: ;
    mptMap: ;
    mptString: writeString(Self.getAsString, pvStream);
    mptInteger: WriteInt(self.getAsInteger, pvStream);
    mptBoolean: ;
    mptFloat: ;
    mptBinary: ;
  end;
end;

procedure TSimpleMsgPack.InnerParseFromStream(pvStream: TStream);
var
  lvByte:Byte;
  lvBytes:TBytes;
  l, i:Cardinal;
  i64:Int64;
begin
  pvStream.Read(lvByte, 1);
  if lvByte <=$7F then   //positive fixint	0xxxxxxx	0x00 - 0x7f
  begin
    setAsInteger(lvByte);
  end else if lvByte <= $8f then //fixmap	1000xxxx	0x80 - 0x8f
  begin
    FDataType := mptMap;
    FChildren.Clear;
    l := lvByte - $80;

    for I := 0 to l - 1 do
    begin
      with InnerAdd do
      begin
        // map key
        InnerParseFromStream(pvStream);
        FName := getAsString;

        // value
        InnerParseFromStream(pvStream);
      end;
    end;
  end else if lvByte <= $9f then //fixarray	1001xxxx	0x90 - 0x9f
  begin

  end else if lvByte <= $bf then //fixstr	101xxxxx	0xa0 - 0xbf
  begin
    l := lvByte - $A0;   // str len
    if l > 0 then
    begin
      SetLength(lvBytes, l + 1);
      lvBytes[l] := 0;

      pvStream.Read(lvBytes[0], l);

      setAsString(UTF8Decode(PAnsiChar(lvBytes)));
    end else
    begin
      setAsString('');
    end;
  end else
  begin
    case lvByte of
      $d9:   //str 8 , 255
      begin
      //  str 8 stores a byte array whose length is upto (2^8)-1 bytes:
      //  +--------+--------+========+
      //  |  0xd9  |YYYYYYYY|  data  |
      //  +--------+--------+========+
        pvStream.Read(lvByte, 1);
        l := lvByte;

        SetLength(lvBytes, l + 1);
        lvBytes[l] := 0;

        pvStream.Read(lvBytes, l);
        setAsString(UTF8Decode(PAnsiChar(lvBytes)));
      end;
      $da:    // str 16
      begin
        //      str 16 stores a byte array whose length is upto (2^16)-1 bytes:
        //      +--------+--------+--------+========+
        //      |  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
        //      +--------+--------+--------+========+

        l := 0; // fill zero
        pvStream.Read(l, 2);
        l := swap16(l);

        SetLength(lvBytes, l + 1);
        lvBytes[l] := 0;
        pvStream.Read(lvBytes, l);
        setAsString(UTF8Decode(PAnsiChar(lvBytes)));
      end;
      $db:    // str 16
      begin
        //  str 32 stores a byte array whose length is upto (2^32)-1 bytes:
        //  +--------+--------+--------+--------+--------+========+
        //  |  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
        //  +--------+--------+--------+--------+--------+========+

        l := 0; // fill zero
        pvStream.Read(l, 4);
        swap32(l);

        SetLength(lvBytes, l + 1);
        lvBytes[l] := 0;
        pvStream.Read(lvBytes, l);
        setAsString(UTF8Decode(PAnsiChar(lvBytes)));
      end;
      $cc, $d0:   //uint 8, int 8
      begin
        //      uint 8 stores a 8-bit unsigned integer
        //      +--------+--------+
        //      |  0xcc  |ZZZZZZZZ|
        //      +--------+--------+
        //      int 8 stores a 8-bit signed integer
        //      +--------+--------+
        //      |  0xd0  |ZZZZZZZZ|
        //      +--------+--------+

        l := 0;
        pvStream.Read(l, 1);
        setAsInteger(l);
      end;
      $cd, $d1:
      begin
        //    uint 16 stores a 16-bit big-endian unsigned integer
        //    +--------+--------+--------+
        //    |  0xcd  |ZZZZZZZZ|ZZZZZZZZ|
        //    +--------+--------+--------+
        //
        //    int 16 stores a 16-bit big-endian signed integer
        //    +--------+--------+--------+
        //    |  0xd1  |ZZZZZZZZ|ZZZZZZZZ|
        //    +--------+--------+--------+

        l := 0;
        pvStream.Read(l, 2);
        l := swap16(l);
        setAsInteger(l);
      end;
    end;
  end;
end;

procedure TSimpleMsgPack.LoadBinaryFromStream(pvStream: TStream; pvLen:
    cardinal = 0);
begin
  if pvLen = 0 then
  begin
    pvStream.Position := 0;
    pvStream.Read(FValue[0], pvStream.Size);
  end else
  begin
    pvStream.ReadBuffer(FValue[0], pvLen);
  end;
end;

procedure TSimpleMsgPack.SaveBinaryToStream(pvStream: TStream);
begin
  pvStream.WriteBuffer(FValue[0], Length(FValue));
end;

procedure TSimpleMsgPack.setAsInteger(pvValue: Int64);
begin
  FDataType := mptInteger;
  SetLength(FValue, SizeOf(Int64));
  PInt64(FValue[0])^ := pvValue;
end;

procedure TSimpleMsgPack.setAsString(pvValue: string);
begin
  FDataType := mptString;
  if SizeOf(Char) = 2 then
  begin
    SetLength(FValue, length(pvValue) shl 1);
    Move(PChar(pvValue)^, FValue[0], Length(FValue));
  end else
  begin
    SetLength(FValue, length(pvValue));
    Move(PChar(pvValue)^, FValue[0], Length(FValue));
  end;
end;

procedure TSimpleMsgPack.SetO(pvPath: String; const Value: TSimpleMsgPack);
begin
  // TODO -cMM: TSimpleMsgPack.SetO default body inserted
end;



end.

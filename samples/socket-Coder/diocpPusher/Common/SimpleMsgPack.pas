unit SimpleMsgPack;

interface

uses
  classes, SysUtils;

type
  TMsgPackType = (mptUnknown, mptMap, mptString, mptInteger, mptBoolean, mptFloat, mptBinary);
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
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;


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

//  function mtoh16(var p:PByte): Word; inline;
//  begin
//    Result := (p^ shl 8);
//    Inc(p);
//    Inc(Result, p^);
//    Inc(p);
//  end;

function swap16(const v): Word;
begin
  Result := PByte(@v)^ shl 8;
  inc(result, PByte(IntPtr(@v) + 1)^);
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

destructor TSimpleMsgPack.Destroy;
begin
  FChildren.Free;
  FChildren := nil;
  inherited Destroy;
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

function TSimpleMsgPack.InnerAdd: TSimpleMsgPack;
begin
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
    mptInteger: ;
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

      pvStream.Read(lvBytes, l);

      setAsString(UTF8Decode(PAnsiChar(lvBytes)^));
    end else
    begin
      setAsString('');
    end;
  end else
  begin
    case lvByte of
      $d9:   //str 8 , 255
      begin
        pvStream.Read(lvByte, 1);
        l := lvByte;

        SetLength(lvBytes, l + 1);
        lvBytes[l] := 0;

        pvStream.Read(lvBytes, l);
        setAsString(UTF8Decode(PAnsiChar(lvBytes)^));
      end;
      $da:
      begin

      end;
    end;
  end;
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
    SetLength(FValue, length(FValue) shr 1);
    Move(PChar(pvValue)^, FValue[0], Length(FValue));
  end else
  begin
    SetLength(FValue, length(FValue));
    Move(PChar(pvValue)^, FValue[0], Length(FValue));
  end;
end;



end.

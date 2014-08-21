unit qdigest;

interface
{
MD5和SHA哈希处理单元
本单元按照自己的想法对接口形式和代码进行了调整，以便更易用。
MD5算法来自于系统自带MessageDigest_5.pas
SHA算法来自DCPcrypt的dcpSHA1.pas,dcpSHA256.pas,dcpSHA512.pas
DCPCrpt版权归http://www.cityinthesky.co.uk/所有
}
{修订日志
2014.8.5
 * 修复了MD5Init时，初始化清零代码错误
2014.8.2
 * 修正了一处Move参数传递错误
}
uses classes, sysutils, qstring;

type
  TQMD5Digest = record
    case Integer of
      0:
        (Value: array [0 .. 15] of Byte);
      1:
        (Id: TGuid);
      2:
        (Low, High: Int64);
  end;

  TQSHADigestType = (sdt160, sdt256, sdt384, sdt512);

  TQSHADigest = record
    HashType: TQSHADigestType;
    case Integer of
      0:
        (SHA160: array [0 .. 19] of Byte);
      1:
        (SHA256: array [0 .. 31] of Byte);
      2:
        (SHA384: array [0 .. 47] of Byte);
      3:
       (SHA512: array [0 .. 63] of Byte);
      4:
        (I32: array [0 .. 7] of Cardinal);
      5:
        (I64: array [0 .. 7] of Int64);
  end;

function MD5Hash(const p: Pointer; len: Integer): TQMD5Digest; overload;
function MD5Hash(const S: QStringW): TQMD5Digest; overload;
function MD5Hash(AStream:TStream):TQMD5Digest;overload;
function MD5File(AFile:QStringW):TQMD5Digest;overload;

function SHA160Hash(const p: Pointer; len: Integer): TQSHADigest;overload;
function SHA160Hash(const S:QStringW): TQSHADigest;overload;
function SHA160Hash(AStream:TStream):TQSHADigest;overload;
function SHA160File(AFileName:QStringW):TQSHADigest;overload;

function SHA256Hash(const p: Pointer; len: Integer): TQSHADigest;overload;
function SHA256Hash(const S:QStringW): TQSHADigest;overload;
function SHA256Hash(AStream:TStream):TQSHADigest;overload;
function SHA256File(AFileName:QStringW):TQSHADigest;overload;

function SHA384Hash(const p: Pointer; len: Integer): TQSHADigest;overload;
function SHA384Hash(const S:QStringW): TQSHADigest;overload;
function SHA384Hash(AStream:TStream):TQSHADigest;overload;
function SHA384File(AFileName:QStringW):TQSHADigest;overload;

function SHA512Hash(const p: Pointer; len: Integer): TQSHADigest;overload;
function SHA512Hash(const S:QStringW): TQSHADigest;overload;
function SHA512Hash(AStream:TStream):TQSHADigest;overload;
function SHA512File(AFileName:QStringW):TQSHADigest;overload;

function DigestToString(const ADigest: TQMD5Digest): QStringW;overload;
function DigestToString(const ADigest:TQSHADigest):QStringW;overload;

implementation

// MD5部分
const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;

  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;

  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;

  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;

var
  MD5Padding: array [0 .. 63] of Byte = (
    $80,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
  );

type
  TLongWords = array of Cardinal;
  TQMD5Count = array [0 .. 1] of Cardinal;
  TQMD5State = array [0 .. 3] of Cardinal;
  TQMD5Block = array [0 .. 15] of Cardinal;
  TQMD5CBits = array [0 .. 7] of Byte;
  TQMD5Buffer = array [0 .. 63] of Byte;

  TQMD5Context = record
    Buffer: TQMD5Buffer; // 64B
    State: TQMD5State; // 16B
    Count: TQMD5Count; // 8B
  end;

  TQSHAContext = record
    CurrentHash: TQSHADigest;
    LenHi, LenLo: longword;
    Index: Cardinal;
    HashBuffer: array [0 .. 127] of Byte;
  end;

procedure MD5Init(var Context: TQMD5Context);
begin
FillChar(Context, SizeOf(TQMD5Context), 0);
Context.State[0] := $67452301;
Context.State[1] := $EFCDAB89;
Context.State[2] := $98BADCFE;
Context.State[3] := $10325476;
end;

procedure MD5Decode(Source, Target: Pointer; Count: longword);
var
  S: PCardinal;
  T: PByte;
  I: longword;
begin
S := Source;
T := Target;
for I := 1 to Count do
  begin
  T^ := S^ and $FF;
  inc(T);
  T^ := (S^ shr 8) and $FF;
  inc(T);
  T^ := (S^ shr 16) and $FF;
  inc(T);
  T^ := (S^ shr 24) and $FF;
  inc(T);
  inc(S);
  end;
end;

// Encode Count bytes at Source into (Count / 4) DWORDs at Target
procedure MD5Encode(Source, Target: Pointer; Count: longword);
var
  S: PByte;
  T: PCardinal;
  I: longword;
begin
S := Source;
T := Target;
for I := 1 to Count div 4 do
  begin
  T^ := S^;
  inc(S);
  T^ := T^ or (S^ shl 8);
  inc(S);
  T^ := T^ or (S^ shl 16);
  inc(S);
  T^ := T^ or (S^ shl 24);
  inc(S);
  inc(T);
  end;
end;

procedure MD5Transform(Buffer: Pointer; var State: TQMD5State);
var
  a, b, c, d: Cardinal;
  Block: TQMD5Block;
  function F(x, y, z: longword): longword;
  begin
  Result := (x and y) or ((not x) and z);
  end;

  function G(x, y, z: longword): longword;
  begin
  Result := (x and z) or (y and (not z));
  end;

  function H(x, y, z: longword): longword;
  begin
  Result := x xor y xor z;
  end;

  function I(x, y, z: longword): longword;
  begin
  Result := y xor (x or (not z));
  end;

  procedure RL(var x: longword; n: Byte);
  begin
  x := (x shl n) or (x shr (32 - n));
  end;

  procedure FF(var a: longword; b, c, d, x: longword; S: Byte; ac: longword);
  begin
  inc(a, F(b, c, d) + x + ac);
  RL(a, S);
  inc(a, b);
  end;

  procedure GG(var a: longword; b, c, d, x: longword; S: Byte; ac: longword);
  begin
  inc(a, G(b, c, d) + x + ac);
  RL(a, S);
  inc(a, b);
  end;

  procedure HH(var a: longword; b, c, d, x: longword; S: Byte; ac: longword);
  begin
  inc(a, H(b, c, d) + x + ac);
  RL(a, S);
  inc(a, b);
  end;

  procedure II(var a: longword; b, c, d, x: longword; S: Byte; ac: longword);
  begin
  inc(a, I(b, c, d) + x + ac);
  RL(a, S);
  inc(a, b);
  end;

begin
MD5Encode(Buffer, @Block, 64);
a := State[0];
b := State[1];
c := State[2];
d := State[3];
FF(a, b, c, d, Block[0], 7, $D76AA478);
FF(d, a, b, c, Block[1], 12, $E8C7B756);
FF(c, d, a, b, Block[2], 17, $242070DB);
FF(b, c, d, a, Block[3], 22, $C1BDCEEE);
FF(a, b, c, d, Block[4], 7, $F57C0FAF);
FF(d, a, b, c, Block[5], 12, $4787C62A);
FF(c, d, a, b, Block[6], 17, $A8304613);
FF(b, c, d, a, Block[7], 22, $FD469501);
FF(a, b, c, d, Block[8], 7, $698098D8);
FF(d, a, b, c, Block[9], 12, $8B44F7AF);
FF(c, d, a, b, Block[10], 17, $FFFF5BB1);
FF(b, c, d, a, Block[11], 22, $895CD7BE);
FF(a, b, c, d, Block[12], 7, $6B901122);
FF(d, a, b, c, Block[13], 12, $FD987193);
FF(c, d, a, b, Block[14], 17, $A679438E);
FF(b, c, d, a, Block[15], 22, $49B40821);
GG(a, b, c, d, Block[1], 5, $F61E2562);
GG(d, a, b, c, Block[6], 9, $C040B340);
GG(c, d, a, b, Block[11], 14, $265E5A51);
GG(b, c, d, a, Block[0], 20, $E9B6C7AA);
GG(a, b, c, d, Block[5], 5, $D62F105D);
GG(d, a, b, c, Block[10], 9, $2441453);
GG(c, d, a, b, Block[15], 14, $D8A1E681);
GG(b, c, d, a, Block[4], 20, $E7D3FBC8);
GG(a, b, c, d, Block[9], 5, $21E1CDE6);
GG(d, a, b, c, Block[14], 9, $C33707D6);
GG(c, d, a, b, Block[3], 14, $F4D50D87);
GG(b, c, d, a, Block[8], 20, $455A14ED);
GG(a, b, c, d, Block[13], 5, $A9E3E905);
GG(d, a, b, c, Block[2], 9, $FCEFA3F8);
GG(c, d, a, b, Block[7], 14, $676F02D9);
GG(b, c, d, a, Block[12], 20, $8D2A4C8A);
HH(a, b, c, d, Block[5], 4, $FFFA3942);
HH(d, a, b, c, Block[8], 11, $8771F681);
HH(c, d, a, b, Block[11], 16, $6D9D6122);
HH(b, c, d, a, Block[14], 23, $FDE5380C);
HH(a, b, c, d, Block[1], 4, $A4BEEA44);
HH(d, a, b, c, Block[4], 11, $4BDECFA9);
HH(c, d, a, b, Block[7], 16, $F6BB4B60);
HH(b, c, d, a, Block[10], 23, $BEBFBC70);
HH(a, b, c, d, Block[13], 4, $289B7EC6);
HH(d, a, b, c, Block[0], 11, $EAA127FA);
HH(c, d, a, b, Block[3], 16, $D4EF3085);
HH(b, c, d, a, Block[6], 23, $4881D05);
HH(a, b, c, d, Block[9], 4, $D9D4D039);
HH(d, a, b, c, Block[12], 11, $E6DB99E5);
HH(c, d, a, b, Block[15], 16, $1FA27CF8);
HH(b, c, d, a, Block[2], 23, $C4AC5665);
II(a, b, c, d, Block[0], 6, $F4292244);
II(d, a, b, c, Block[7], 10, $432AFF97);
II(c, d, a, b, Block[14], 15, $AB9423A7);
II(b, c, d, a, Block[5], 21, $FC93A039);
II(a, b, c, d, Block[12], 6, $655B59C3);
II(d, a, b, c, Block[3], 10, $8F0CCC92);
II(c, d, a, b, Block[10], 15, $FFEFF47D);
II(b, c, d, a, Block[1], 21, $85845DD1);
II(a, b, c, d, Block[8], 6, $6FA87E4F);
II(d, a, b, c, Block[15], 10, $FE2CE6E0);
II(c, d, a, b, Block[6], 15, $A3014314);
II(b, c, d, a, Block[13], 21, $4E0811A1);
II(a, b, c, d, Block[4], 6, $F7537E82);
II(d, a, b, c, Block[11], 10, $BD3AF235);
II(c, d, a, b, Block[2], 15, $2AD7D2BB);
II(b, c, d, a, Block[9], 21, $EB86D391);
inc(State[0], a);
inc(State[1], b);
inc(State[2], c);
inc(State[3], d);
end;

procedure MD5Update(var Context: TQMD5Context; Input: PByte; Length: longword);
var
  Index: longword;
  PartLen: longword;
  I: longword;
begin
with Context do
  begin
  Index := (Count[0] shr 3) and $3F;
  inc(Count[0], Length shl 3);
  if Count[0] < (Length shl 3) then
    inc(Count[1]);
  inc(Count[1], Length shr 29);
  end;
PartLen := 64 - Index;
if Length >= PartLen then
  begin
  Move(Input^, Context.Buffer[Index], PartLen);
  MD5Transform(@Context.Buffer[0], Context.State);
  I := PartLen;
  while I + 63 < Length do
    begin
    MD5Transform(@Input[I], Context.State);
    inc(I, 64);
    end;
  Index := 0;
  end
else
  I := 0;
Move(Input[I], Context.Buffer[Index], Length - I);
end;

procedure MD5Final(var Context: TQMD5Context; var Digest: TQMD5Digest);
var
  Bits: TQMD5CBits;
  Index: longword;
  PadLen: longword;
begin
MD5Decode(@Context.Count, @Bits, 2);
Index := (Context.Count[0] shr 3) and $3F;
if Index < 56 then
  PadLen := 56 - Index
else
  PadLen := 120 - Index;
MD5Update(Context, @MD5Padding, PadLen);
MD5Update(Context, @Bits, 8);
MD5Decode(@Context.State, @Digest, 4);
end;

function MD5Hash(const p: Pointer; len: Integer): TQMD5Digest;
var
  AContext: TQMD5Context;
begin
MD5Init(AContext);
MD5Update(AContext, p, len);
MD5Final(AContext, Result);
end;

function MD5Hash(const S: QStringW): TQMD5Digest;
var
  T: QStringA;
begin
T := qstring.Utf8Encode(S);
Result := MD5Hash(PQCharA(T), T.Length);
end;

function MD5Hash(AStream:TStream):TQMD5Digest;
var
  ABuf:array[0..65535] of Byte;
  AReaded:Integer;
  AContext: TQMD5Context;
begin
MD5Init(AContext);
AStream.Position:=0;
repeat
  AReaded:=AStream.Read(ABuf,65536);
  if AReaded>0 then
    MD5Update(AContext,@ABuf[0],AReaded);
until AReaded=0;
MD5Final(AContext,Result);
end;

function MD5File(AFile:QStringW):TQMD5Digest;
var
  AStream:TFileStream;
begin
AStream:=TFileStream.Create(AFile,fmOpenRead or fmShareDenyWrite);
try
  Result:=MD5Hash(AStream);
finally
  FreeObject(AStream);
end;
end;

function DigestToString(const ADigest: TQMD5Digest): String;
begin
Result := qstring.BinToHex(@ADigest, SizeOf(TQMD5Digest));
end;

// SHA部分
procedure SHAInit(var AContext: TQSHAContext; AType: TQSHADigestType);
begin
FillChar(AContext, SizeOf(AContext), 0);
AContext.CurrentHash.HashType := AType;
case AType of
  sdt160:
    begin
    AContext.CurrentHash.I32[0] := $67452301;
    AContext.CurrentHash.I32[1] := $EFCDAB89;
    AContext.CurrentHash.I32[2] := $98BADCFE;
    AContext.CurrentHash.I32[3] := $10325476;
    AContext.CurrentHash.I32[4] := $C3D2E1F0;
    end;
  sdt256:
    begin
    AContext.CurrentHash.I32[0] := $6A09E667;
    AContext.CurrentHash.I32[1] := $BB67AE85;
    AContext.CurrentHash.I32[2] := $3C6EF372;
    AContext.CurrentHash.I32[3] := $A54FF53A;
    AContext.CurrentHash.I32[4] := $510E527F;
    AContext.CurrentHash.I32[5] := $9B05688C;
    AContext.CurrentHash.I32[6] := $1F83D9AB;
    AContext.CurrentHash.I32[7] := $5BE0CD19;
    end;
  sdt384:
    begin
    AContext.CurrentHash.I64[0] := Int64($CBBB9D5DC1059ED8);
    AContext.CurrentHash.I64[1] := Int64($629A292A367CD507);
    AContext.CurrentHash.I64[2] := Int64($9159015A3070DD17);
    AContext.CurrentHash.I64[3] := Int64($152FECD8F70E5939);
    AContext.CurrentHash.I64[4] := Int64($67332667FFC00B31);
    AContext.CurrentHash.I64[5] := Int64($8EB44A8768581511);
    AContext.CurrentHash.I64[6] := Int64($DB0C2E0D64F98FA7);
    AContext.CurrentHash.I64[7] := Int64($47B5481DBEFA4FA4);
    end;
  sdt512:
    begin
    AContext.CurrentHash.I64[0] := Int64($6A09E667F3BCC908);
    AContext.CurrentHash.I64[1] := Int64($BB67AE8584CAA73B);
    AContext.CurrentHash.I64[2] := Int64($3C6EF372FE94F82B);
    AContext.CurrentHash.I64[3] := Int64($A54FF53A5F1D36F1);
    AContext.CurrentHash.I64[4] := Int64($510E527FADE682D1);
    AContext.CurrentHash.I64[5] := Int64($9B05688C2B3E6C1F);
    AContext.CurrentHash.I64[6] := Int64($1F83D9ABFB41BD6B);
    AContext.CurrentHash.I64[7] := Int64($5BE0CD19137E2179);
    end;
end;
end;

function SwapDWord(a: longword): longword; inline;
begin
Result := ((a and $FF) shl 24) or ((a and $FF00) shl 8) or
  ((a and $FF0000) shr 8) or ((a and $FF000000) shr 24);
end;

function SwapQWord(a: Int64): Int64; inline;
begin
Result := ((a and $FF) shl 56) or ((a and $FF00) shl 40) or
  ((a and $FF0000) shl 24) or ((a and $FF000000) shl 8) or
  ((a and $FF00000000) shr 8) or ((a and $FF0000000000) shr 24) or
  ((a and $FF000000000000) shr 40) or ((a and $FF00000000000000) shr 56);
end;

procedure SHACompress(var AContext: TQSHAContext);
  procedure SHA160Compress;
  var
    a, b, c, d, E: longword;
    W: array [0 .. 79] of longword;
    I: longword;
  begin
  AContext.Index := 0;
  FillChar(W, SizeOf(W), 0);
  Move(AContext.HashBuffer, W, 64);
  for I := 0 to 15 do
    W[I] := SwapDWord(W[I]);
  for I := 16 to 79 do
    W[I] := ((W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16]) shl 1) or
      ((W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16]) shr 31);
  a := AContext.CurrentHash.I32[0];
  b := AContext.CurrentHash.I32[1];
  c := AContext.CurrentHash.I32[2];
  d := AContext.CurrentHash.I32[3];
  E := AContext.CurrentHash.I32[4];
  inc(E, ((a shl 5) or (a shr 27)) + (d xor (b and (c xor d))) +
    $5A827999 + W[0]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (c xor (a and (b xor c))) +
    $5A827999 + W[1]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (b xor (E and (a xor b))) +
    $5A827999 + W[2]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (a xor (d and (E xor a))) +
    $5A827999 + W[3]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (E xor (c and (d xor E))) +
    $5A827999 + W[4]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (d xor (b and (c xor d))) +
    $5A827999 + W[5]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (c xor (a and (b xor c))) +
    $5A827999 + W[6]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (b xor (E and (a xor b))) +
    $5A827999 + W[7]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (a xor (d and (E xor a))) +
    $5A827999 + W[8]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (E xor (c and (d xor E))) +
    $5A827999 + W[9]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (d xor (b and (c xor d))) +
    $5A827999 + W[10]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (c xor (a and (b xor c))) +
    $5A827999 + W[11]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (b xor (E and (a xor b))) +
    $5A827999 + W[12]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (a xor (d and (E xor a))) +
    $5A827999 + W[13]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (E xor (c and (d xor E))) +
    $5A827999 + W[14]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (d xor (b and (c xor d))) +
    $5A827999 + W[15]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (c xor (a and (b xor c))) +
    $5A827999 + W[16]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (b xor (E and (a xor b))) +
    $5A827999 + W[17]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (a xor (d and (E xor a))) +
    $5A827999 + W[18]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (E xor (c and (d xor E))) +
    $5A827999 + W[19]);
  c := (c shl 30) or (c shr 2);

  inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $6ED9EBA1 + W[20]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $6ED9EBA1 + W[21]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $6ED9EBA1 + W[22]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $6ED9EBA1 + W[23]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $6ED9EBA1 + W[24]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $6ED9EBA1 + W[25]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $6ED9EBA1 + W[26]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $6ED9EBA1 + W[27]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $6ED9EBA1 + W[28]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $6ED9EBA1 + W[29]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $6ED9EBA1 + W[30]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $6ED9EBA1 + W[31]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $6ED9EBA1 + W[32]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $6ED9EBA1 + W[33]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $6ED9EBA1 + W[34]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $6ED9EBA1 + W[35]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $6ED9EBA1 + W[36]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $6ED9EBA1 + W[37]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $6ED9EBA1 + W[38]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $6ED9EBA1 + W[39]);
  c := (c shl 30) or (c shr 2);

  inc(E, ((a shl 5) or (a shr 27)) + ((b and c) or (d and (b or c))) +
    $8F1BBCDC + W[40]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + ((a and b) or (c and (a or b))) +
    $8F1BBCDC + W[41]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + ((E and a) or (b and (E or a))) +
    $8F1BBCDC + W[42]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + ((d and E) or (a and (d or E))) +
    $8F1BBCDC + W[43]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + ((c and d) or (E and (c or d))) +
    $8F1BBCDC + W[44]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + ((b and c) or (d and (b or c))) +
    $8F1BBCDC + W[45]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + ((a and b) or (c and (a or b))) +
    $8F1BBCDC + W[46]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + ((E and a) or (b and (E or a))) +
    $8F1BBCDC + W[47]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + ((d and E) or (a and (d or E))) +
    $8F1BBCDC + W[48]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + ((c and d) or (E and (c or d))) +
    $8F1BBCDC + W[49]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + ((b and c) or (d and (b or c))) +
    $8F1BBCDC + W[50]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + ((a and b) or (c and (a or b))) +
    $8F1BBCDC + W[51]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + ((E and a) or (b and (E or a))) +
    $8F1BBCDC + W[52]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + ((d and E) or (a and (d or E))) +
    $8F1BBCDC + W[53]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + ((c and d) or (E and (c or d))) +
    $8F1BBCDC + W[54]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + ((b and c) or (d and (b or c))) +
    $8F1BBCDC + W[55]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + ((a and b) or (c and (a or b))) +
    $8F1BBCDC + W[56]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + ((E and a) or (b and (E or a))) +
    $8F1BBCDC + W[57]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + ((d and E) or (a and (d or E))) +
    $8F1BBCDC + W[58]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + ((c and d) or (E and (c or d))) +
    $8F1BBCDC + W[59]);
  c := (c shl 30) or (c shr 2);

  inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $CA62C1D6 + W[60]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $CA62C1D6 + W[61]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $CA62C1D6 + W[62]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $CA62C1D6 + W[63]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $CA62C1D6 + W[64]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $CA62C1D6 + W[65]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $CA62C1D6 + W[66]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $CA62C1D6 + W[67]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $CA62C1D6 + W[68]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $CA62C1D6 + W[69]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $CA62C1D6 + W[70]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $CA62C1D6 + W[71]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $CA62C1D6 + W[72]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $CA62C1D6 + W[73]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $CA62C1D6 + W[74]);
  c := (c shl 30) or (c shr 2);
  inc(E, ((a shl 5) or (a shr 27)) + (b xor c xor d) + $CA62C1D6 + W[75]);
  b := (b shl 30) or (b shr 2);
  inc(d, ((E shl 5) or (E shr 27)) + (a xor b xor c) + $CA62C1D6 + W[76]);
  a := (a shl 30) or (a shr 2);
  inc(c, ((d shl 5) or (d shr 27)) + (E xor a xor b) + $CA62C1D6 + W[77]);
  E := (E shl 30) or (E shr 2);
  inc(b, ((c shl 5) or (c shr 27)) + (d xor E xor a) + $CA62C1D6 + W[78]);
  d := (d shl 30) or (d shr 2);
  inc(a, ((b shl 5) or (b shr 27)) + (c xor d xor E) + $CA62C1D6 + W[79]);
  c := (c shl 30) or (c shr 2);
  AContext.CurrentHash.I32[0] := AContext.CurrentHash.I32[0] + a;
  AContext.CurrentHash.I32[1] := AContext.CurrentHash.I32[1] + b;
  AContext.CurrentHash.I32[2] := AContext.CurrentHash.I32[2] + c;
  AContext.CurrentHash.I32[3] := AContext.CurrentHash.I32[3] + d;
  AContext.CurrentHash.I32[4] := AContext.CurrentHash.I32[4] + E;
  FillChar(AContext.HashBuffer, 64, 0);
  end;
  procedure SHA256Compress;
  var
    a, b, c, d, E, F, G, H, t1, t2: longword;
    W: array [0 .. 63] of longword;
    I: longword;
  begin
  AContext.Index := 0;
  FillChar(W, SizeOf(W), 0);
  a := AContext.CurrentHash.I32[0];
  b := AContext.CurrentHash.I32[1];
  c := AContext.CurrentHash.I32[2];
  d := AContext.CurrentHash.I32[3];
  E := AContext.CurrentHash.I32[4];
  F := AContext.CurrentHash.I32[5];
  G := AContext.CurrentHash.I32[6];
  H := AContext.CurrentHash.I32[7];
  Move(AContext.HashBuffer, W, 64);
  for I := 0 to 15 do
    W[I] := SwapDWord(W[I]);
  for I := 16 to 63 do
    W[I] := (((W[I - 2] shr 17) or (W[I - 2] shl 15)) xor ((W[I - 2] shr 19) or
      (W[I - 2] shl 13)) xor (W[I - 2] shr 10)) + W[I - 7] +
      (((W[I - 15] shr 7) or (W[I - 15] shl 25)) xor ((W[I - 15] shr 18) or
      (W[I - 15] shl 14)) xor (W[I - 15] shr 3)) + W[I - 16];
  t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
    xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
    $428A2F98 + W[0];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
    xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  H := t1 + t2;
  d := d + t1;
  t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
    xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
    $71374491 + W[1];
  t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
    xor ((H shr 22) xor (H shl 10))) + ((H and a) xor (H and b) xor (a and b));
  G := t1 + t2;
  c := c + t1;
  t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
    xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
    $B5C0FBCF + W[2];
  t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and a) xor (H and a));
  F := t1 + t2;
  b := b + t1;
  t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
    xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
    $E9B5DBA5 + W[3];
  t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
    xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
    $3956C25B + W[4];
  t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  d := t1 + t2;
  H := H + t1;
  t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
    xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
    $59F111F1 + W[5];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
    xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and F) xor (E and F));
  c := t1 + t2;
  G := G + t1;
  t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
    xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
    $923F82A4 + W[6];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
    xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := t1 + t2;
  F := F + t1;
  t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
    xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
    $AB1C5ED5 + W[7];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
    xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  E := E + t1;
  t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
    xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
    $D807AA98 + W[8];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
    xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  H := t1 + t2;
  d := d + t1;
  t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
    xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
    $12835B01 + W[9];
  t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
    xor ((H shr 22) xor (H shl 10))) + ((H and a) xor (H and b) xor (a and b));
  G := t1 + t2;
  c := c + t1;
  t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
    xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
    $243185BE + W[10];
  t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and a) xor (H and a));
  F := t1 + t2;
  b := b + t1;
  t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
    xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
    $550C7DC3 + W[11];
  t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
    xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
    $72BE5D74 + W[12];
  t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  d := t1 + t2;
  H := H + t1;
  t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
    xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
    $80DEB1FE + W[13];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
    xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and F) xor (E and F));
  c := t1 + t2;
  G := G + t1;
  t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
    xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
    $9BDC06A7 + W[14];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
    xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := t1 + t2;
  F := F + t1;
  t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
    xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
    $C19BF174 + W[15];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
    xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  E := E + t1;
  t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
    xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
    $E49B69C1 + W[16];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
    xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  H := t1 + t2;
  d := d + t1;
  t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
    xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
    $EFBE4786 + W[17];
  t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
    xor ((H shr 22) xor (H shl 10))) + ((H and a) xor (H and b) xor (a and b));
  G := t1 + t2;
  c := c + t1;
  t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
    xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
    $0FC19DC6 + W[18];
  t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and a) xor (H and a));
  F := t1 + t2;
  b := b + t1;
  t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
    xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
    $240CA1CC + W[19];
  t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
    xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
    $2DE92C6F + W[20];
  t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  d := t1 + t2;
  H := H + t1;
  t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
    xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
    $4A7484AA + W[21];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
    xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and F) xor (E and F));
  c := t1 + t2;
  G := G + t1;
  t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
    xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
    $5CB0A9DC + W[22];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
    xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := t1 + t2;
  F := F + t1;
  t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
    xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
    $76F988DA + W[23];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
    xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  E := E + t1;
  t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
    xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
    $983E5152 + W[24];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
    xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  H := t1 + t2;
  d := d + t1;
  t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
    xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
    $A831C66D + W[25];
  t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
    xor ((H shr 22) xor (H shl 10))) + ((H and a) xor (H and b) xor (a and b));
  G := t1 + t2;
  c := c + t1;
  t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
    xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
    $B00327C8 + W[26];
  t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and a) xor (H and a));
  F := t1 + t2;
  b := b + t1;
  t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
    xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
    $BF597FC7 + W[27];
  t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
    xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
    $C6E00BF3 + W[28];
  t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  d := t1 + t2;
  H := H + t1;
  t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
    xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
    $D5A79147 + W[29];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
    xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and F) xor (E and F));
  c := t1 + t2;
  G := G + t1;
  t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
    xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
    $06CA6351 + W[30];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
    xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := t1 + t2;
  F := F + t1;
  t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
    xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
    $14292967 + W[31];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
    xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  E := E + t1;
  t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
    xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
    $27B70A85 + W[32];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
    xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  H := t1 + t2;
  d := d + t1;
  t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
    xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
    $2E1B2138 + W[33];
  t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
    xor ((H shr 22) xor (H shl 10))) + ((H and a) xor (H and b) xor (a and b));
  G := t1 + t2;
  c := c + t1;
  t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
    xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
    $4D2C6DFC + W[34];
  t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and a) xor (H and a));
  F := t1 + t2;
  b := b + t1;
  t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
    xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
    $53380D13 + W[35];
  t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
    xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
    $650A7354 + W[36];
  t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  d := t1 + t2;
  H := H + t1;
  t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
    xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
    $766A0ABB + W[37];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
    xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and F) xor (E and F));
  c := t1 + t2;
  G := G + t1;
  t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
    xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
    $81C2C92E + W[38];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
    xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := t1 + t2;
  F := F + t1;
  t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
    xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
    $92722C85 + W[39];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
    xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  E := E + t1;
  t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
    xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
    $A2BFE8A1 + W[40];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
    xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  H := t1 + t2;
  d := d + t1;
  t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
    xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
    $A81A664B + W[41];
  t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
    xor ((H shr 22) xor (H shl 10))) + ((H and a) xor (H and b) xor (a and b));
  G := t1 + t2;
  c := c + t1;
  t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
    xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
    $C24B8B70 + W[42];
  t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and a) xor (H and a));
  F := t1 + t2;
  b := b + t1;
  t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
    xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
    $C76C51A3 + W[43];
  t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
    xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
    $D192E819 + W[44];
  t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  d := t1 + t2;
  H := H + t1;
  t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
    xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
    $D6990624 + W[45];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
    xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and F) xor (E and F));
  c := t1 + t2;
  G := G + t1;
  t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
    xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
    $F40E3585 + W[46];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
    xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := t1 + t2;
  F := F + t1;
  t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
    xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
    $106AA070 + W[47];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
    xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  E := E + t1;
  t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
    xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
    $19A4C116 + W[48];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
    xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  H := t1 + t2;
  d := d + t1;
  t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
    xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
    $1E376C08 + W[49];
  t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
    xor ((H shr 22) xor (H shl 10))) + ((H and a) xor (H and b) xor (a and b));
  G := t1 + t2;
  c := c + t1;
  t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
    xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
    $2748774C + W[50];
  t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and a) xor (H and a));
  F := t1 + t2;
  b := b + t1;
  t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
    xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
    $34B0BCB5 + W[51];
  t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
    xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
    $391C0CB3 + W[52];
  t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  d := t1 + t2;
  H := H + t1;
  t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
    xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
    $4ED8AA4A + W[53];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
    xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and F) xor (E and F));
  c := t1 + t2;
  G := G + t1;
  t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
    xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
    $5B9CCA4F + W[54];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
    xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := t1 + t2;
  F := F + t1;
  t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
    xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
    $682E6FF3 + W[55];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
    xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  E := E + t1;
  t1 := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21))
    xor ((E shr 25) or (E shl 7))) + ((E and F) xor (not E and G)) +
    $748F82EE + W[56];
  t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19))
    xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  H := t1 + t2;
  d := d + t1;
  t1 := G + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21))
    xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and F)) +
    $78A5636F + W[57];
  t2 := (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19))
    xor ((H shr 22) xor (H shl 10))) + ((H and a) xor (H and b) xor (a and b));
  G := t1 + t2;
  c := c + t1;
  t1 := F + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21))
    xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) +
    $84C87814 + W[58];
  t2 := (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19))
    xor ((G shr 22) xor (G shl 10))) + ((G and H) xor (G and a) xor (H and a));
  F := t1 + t2;
  b := b + t1;
  t1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21))
    xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) +
    $8CC70208 + W[59];
  t2 := (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19))
    xor ((F shr 22) xor (F shl 10))) + ((F and G) xor (F and H) xor (G and H));
  E := t1 + t2;
  a := a + t1;
  t1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21))
    xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) +
    $90BEFFFA + W[60];
  t2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19))
    xor ((E shr 22) xor (E shl 10))) + ((E and F) xor (E and G) xor (F and G));
  d := t1 + t2;
  H := H + t1;
  t1 := c + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21))
    xor ((H shr 25) or (H shl 7))) + ((H and a) xor (not H and b)) +
    $A4506CEB + W[61];
  t2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19))
    xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and F) xor (E and F));
  c := t1 + t2;
  G := G + t1;
  t1 := b + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21))
    xor ((G shr 25) or (G shl 7))) + ((G and H) xor (not G and a)) +
    $BEF9A3F7 + W[62];
  t2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19))
    xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := t1 + t2;
  F := F + t1;
  t1 := a + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21))
    xor ((F shr 25) or (F shl 7))) + ((F and G) xor (not F and H)) +
    $C67178F2 + W[63];
  t2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19))
    xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := t1 + t2;
  E := E + t1;
  AContext.CurrentHash.I32[0] := AContext.CurrentHash.I32[0] + a;
  AContext.CurrentHash.I32[1] := AContext.CurrentHash.I32[1] + b;
  AContext.CurrentHash.I32[2] := AContext.CurrentHash.I32[2] + c;
  AContext.CurrentHash.I32[3] := AContext.CurrentHash.I32[3] + d;
  AContext.CurrentHash.I32[4] := AContext.CurrentHash.I32[4] + E;
  AContext.CurrentHash.I32[5] := AContext.CurrentHash.I32[5] + F;
  AContext.CurrentHash.I32[6] := AContext.CurrentHash.I32[6] + G;
  AContext.CurrentHash.I32[7] := AContext.CurrentHash.I32[7] + H;
  FillChar(AContext.HashBuffer, 64, 0);
  end;
  procedure SHA384Compress;
  var
    a, b, c, d, E, F, G, H, t1, t2: Int64;
    W: array [0 .. 79] of Int64;
    I: longword;
  begin
  AContext.Index := 0;
  FillChar(W, SizeOf(W), 0);
  a := AContext.CurrentHash.I64[0];
  b := AContext.CurrentHash.I64[1];
  c := AContext.CurrentHash.I64[2];
  d := AContext.CurrentHash.I64[3];
  E := AContext.CurrentHash.I64[4];
  F := AContext.CurrentHash.I64[5];
  G := AContext.CurrentHash.I64[6];
  H := AContext.CurrentHash.I64[7];
  Move(AContext.HashBuffer, W, 128);
  for I := 0 to 15 do
    W[I] := SwapDWord(W[I]);
  for I := 16 to 79 do
    W[I] := (((W[I - 2] shr 19) or (W[I - 2] shl 45)) xor ((W[I - 2] shr 61) or
      (W[I - 2] shl 3)) xor (W[I - 2] shr 6)) + W[I - 7] +
      (((W[I - 15] shr 1) or (W[I - 15] shl 63)) xor ((W[I - 15] shr 8) or
      (W[I - 15] shl 56)) xor (W[I - 15] shr 7)) + W[I - 16];
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $428A2F98D728AE22 + W[0];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $7137449123EF65CD + W[1];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $B5C0FBCFEC4D3B2F + W[2];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $E9B5DBA58189DBBC + W[3];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $3956C25BF348B538 + W[4];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $59F111F1B605D019 + W[5];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $923F82A4AF194F9B + W[6];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $AB1C5ED5DA6D8118 + W[7];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $D807AA98A3030242 + W[8];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $12835B0145706FBE + W[9];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $243185BE4EE4B28C + W[10];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $550C7DC3D5FFB4E2 + W[11];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $72BE5D74F27B896F + W[12];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $80DEB1FE3B1696B1 + W[13];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $9BDC06A725C71235 + W[14];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $C19BF174CF692694 + W[15];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $E49B69C19EF14AD2 + W[16];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $EFBE4786384F25E3 + W[17];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $0FC19DC68B8CD5B5 + W[18];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $240CA1CC77AC9C65 + W[19];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $2DE92C6F592B0275 + W[20];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $4A7484AA6EA6E483 + W[21];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $5CB0A9DCBD41FBD4 + W[22];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $76F988DA831153B5 + W[23];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $983E5152EE66DFAB + W[24];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $A831C66D2DB43210 + W[25];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $B00327C898FB213F + W[26];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $BF597FC7BEEF0EE4 + W[27];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $C6E00BF33DA88FC2 + W[28];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $D5A79147930AA725 + W[29];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $06CA6351E003826F + W[30];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $142929670A0E6E70 + W[31];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $27B70A8546D22FFC + W[32];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $2E1B21385C26C926 + W[33];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $4D2C6DFC5AC42AED + W[34];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $53380D139D95B3DF + W[35];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $650A73548BAF63DE + W[36];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $766A0ABB3C77B2A8 + W[37];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $81C2C92E47EDAEE6 + W[38];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $92722C851482353B + W[39];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $A2BFE8A14CF10364 + W[40];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $A81A664BBC423001 + W[41];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $C24B8B70D0F89791 + W[42];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $C76C51A30654BE30 + W[43];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $D192E819D6EF5218 + W[44];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $D69906245565A910 + W[45];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $F40E35855771202A + W[46];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $106AA07032BBD1B8 + W[47];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $19A4C116B8D2D0C8 + W[48];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $1E376C085141AB53 + W[49];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $2748774CDF8EEB99 + W[50];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $34B0BCB5E19B48A8 + W[51];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $391C0CB3C5C95A63 + W[52];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $4ED8AA4AE3418ACB + W[53];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $5B9CCA4F7763E373 + W[54];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $682E6FF3D6B2B8A3 + W[55];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $748F82EE5DEFB2FC + W[56];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $78A5636F43172F60 + W[57];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $84C87814A1F0AB72 + W[58];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $8CC702081A6439EC + W[59];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $90BEFFFA23631E28 + W[60];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $A4506CEBDE82BDE9 + W[61];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $BEF9A3F7B2C67915 + W[62];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $C67178F2E372532B + W[63];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $CA273ECEEA26619C + W[64];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $D186B8C721C0C207 + W[65];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $EADA7DD6CDE0EB1E + W[66];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $F57D4F7FEE6ED178 + W[67];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $06F067AA72176FBA + W[68];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $0A637DC5A2C898A6 + W[69];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $113F9804BEF90DAE + W[70];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $1B710B35131C471B + W[71];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;
  t1 := H + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46))
    xor ((E shr 41) or (E shl 23))) + ((E and F) xor (not E and G)) +
    $28DB77F523047D84 + W[72];
  t2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30))
    xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + t1;
  H := t1 + t2;
  t1 := G + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46))
    xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and F)) +
    $32CAAB7B40C72493 + W[73];
  t2 := (((H shr 28) or (H shl 36)) xor ((H shr 34) or (H shl 30))
    xor ((H shr 39) or (H shl 25))) + ((H and a) xor (H and b) xor (a and b));
  c := c + t1;
  G := t1 + t2;
  t1 := F + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46))
    xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) +
    $3C9EBE0A15C9BEBC + W[74];
  t2 := (((G shr 28) or (G shl 36)) xor ((G shr 34) or (G shl 30))
    xor ((G shr 39) or (G shl 25))) + ((G and H) xor (G and a) xor (H and a));
  b := b + t1;
  F := t1 + t2;
  t1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46))
    xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) +
    $431D67C49C100D4C + W[75];
  t2 := (((F shr 28) or (F shl 36)) xor ((F shr 34) or (F shl 30))
    xor ((F shr 39) or (F shl 25))) + ((F and G) xor (F and H) xor (G and H));
  a := a + t1;
  E := t1 + t2;
  t1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46))
    xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) +
    $4CC5D4BECB3E42B6 + W[76];
  t2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30))
    xor ((E shr 39) or (E shl 25))) + ((E and F) xor (E and G) xor (F and G));
  H := H + t1;
  d := t1 + t2;
  t1 := c + (((H shr 14) or (H shl 50)) xor ((H shr 18) or (H shl 46))
    xor ((H shr 41) or (H shl 23))) + ((H and a) xor (not H and b)) +
    $597F299CFC657E2A + W[77];
  t2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30))
    xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and F) xor (E and F));
  G := G + t1;
  c := t1 + t2;
  t1 := b + (((G shr 14) or (G shl 50)) xor ((G shr 18) or (G shl 46))
    xor ((G shr 41) or (G shl 23))) + ((G and H) xor (not G and a)) +
    $5FCB6FAB3AD6FAEC + W[78];
  t2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30))
    xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  F := F + t1;
  b := t1 + t2;
  t1 := a + (((F shr 14) or (F shl 50)) xor ((F shr 18) or (F shl 46))
    xor ((F shr 41) or (F shl 23))) + ((F and G) xor (not F and H)) +
    $6C44198C4A475817 + W[79];
  t2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30))
    xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + t1;
  a := t1 + t2;

  AContext.CurrentHash.I64[0] := AContext.CurrentHash.I64[0] + a;
  AContext.CurrentHash.I64[1] := AContext.CurrentHash.I64[1] + b;
  AContext.CurrentHash.I64[2] := AContext.CurrentHash.I64[2] + c;
  AContext.CurrentHash.I64[3] := AContext.CurrentHash.I64[3] + d;
  AContext.CurrentHash.I64[4] := AContext.CurrentHash.I64[4] + E;
  AContext.CurrentHash.I64[5] := AContext.CurrentHash.I64[5] + F;
  AContext.CurrentHash.I64[6] := AContext.CurrentHash.I64[6] + G;
  AContext.CurrentHash.I64[7] := AContext.CurrentHash.I64[7] + H;
  FillChar(AContext.HashBuffer, 128, 0);
  end;

begin
case AContext.CurrentHash.HashType of
  sdt160:
    SHA160Compress;
  sdt256:
    SHA256Compress;
  sdt384, sdt512:
    SHA384Compress;
end;
end;

procedure SHAUpdate(var AContext: TQSHAContext; const p: Pointer;
  ASize: longword);
var
  pBuf: PByte;
  procedure SHA160Update;
  begin
  inc(AContext.LenHi, ASize shr 29);
  inc(AContext.LenLo, ASize shl 3);
  if AContext.LenLo < (ASize shl 3) then
    inc(AContext.LenHi);
  pBuf := p;
  while ASize > 0 do
    begin
    if (64 - AContext.Index) <= ASize then
      begin
      Move(pBuf^, AContext.HashBuffer[AContext.Index], 64 - AContext.Index);
      Dec(ASize, 64 - AContext.Index);
      inc(pBuf, 64 - AContext.Index);
      SHACompress(AContext);
      end
    else
      begin
      Move(pBuf^, AContext.HashBuffer[AContext.Index], ASize);
      inc(AContext.Index, ASize);
      ASize := 0;
      end;
    end;
  end;
  procedure SHA256Update;
  begin
  inc(AContext.LenHi, ASize shr 29);
  inc(AContext.LenLo, ASize shl 3);
  if AContext.LenLo < (ASize shl 3) then
    inc(AContext.LenHi);
  pBuf := p;
  while ASize > 0 do
    begin
    if (64 - AContext.Index) <= ASize then
      begin
      Move(pBuf^, AContext.HashBuffer[AContext.Index], 64 - AContext.Index);
      Dec(ASize, 64 - AContext.Index);
      inc(pBuf, 64 - AContext.Index);
      SHACompress(AContext);
      end
    else
      begin
      Move(pBuf^, AContext.HashBuffer[AContext.Index], ASize);
      inc(AContext.Index, ASize);
      ASize := 0;
      end;
    end;
  end;
  procedure SHA384Update;
  begin
  inc(AContext.LenLo, ASize shl 3);
  if AContext.LenLo < (ASize shl 3) then
    inc(AContext.LenHi);
  pBuf := p;
  while ASize > 0 do
    begin
    if (128 - AContext.Index) <= ASize then
      begin
      Move(pBuf^, AContext.HashBuffer[AContext.Index], 128 - AContext.Index);
      Dec(ASize, 128 - AContext.Index);
      inc(pBuf, 128 - AContext.Index);
      SHACompress(AContext);
      end
    else
      begin
      Move(pBuf^, AContext.HashBuffer[AContext.Index], ASize);
      inc(AContext.Index, ASize);
      ASize := 0;
      end;
    end;
  end;

  procedure SHA512Update;
  begin
  SHA384Update;
  end;

begin
case AContext.CurrentHash.HashType of
  sdt160:
    SHA160Update;
  sdt256:
    SHA256Update;
  sdt384:
    SHA384Update;
  sdt512:
    SHA512Update;
end;
end;

procedure SHAFinal(var AContext: TQSHAContext);
  procedure SHA160Final;
  begin
  AContext.HashBuffer[AContext.Index] := $80;
  if AContext.Index >= 56 then
    SHACompress(AContext);
  PLongWord(@AContext.HashBuffer[56])^ := SwapDWord(AContext.LenHi);
  PLongWord(@AContext.HashBuffer[60])^ := SwapDWord(AContext.LenLo);
  SHACompress(AContext);
  AContext.CurrentHash.I32[0] := SwapDWord(AContext.CurrentHash.I32[0]);
  AContext.CurrentHash.I32[1] := SwapDWord(AContext.CurrentHash.I32[1]);
  AContext.CurrentHash.I32[2] := SwapDWord(AContext.CurrentHash.I32[2]);
  AContext.CurrentHash.I32[3] := SwapDWord(AContext.CurrentHash.I32[3]);
  AContext.CurrentHash.I32[4] := SwapDWord(AContext.CurrentHash.I32[4]);
  end;
  procedure SHA256Final;
  begin
  AContext.HashBuffer[AContext.Index] := $80;
  if AContext.Index >= 56 then
    SHACompress(AContext);
  PLongWord(@AContext.HashBuffer[56])^ := SwapDWord(AContext.LenHi);
  PLongWord(@AContext.HashBuffer[60])^ := SwapDWord(AContext.LenLo);
  SHACompress(AContext);
  AContext.CurrentHash.I32[0] := SwapDWord(AContext.CurrentHash.I32[0]);
  AContext.CurrentHash.I32[1] := SwapDWord(AContext.CurrentHash.I32[1]);
  AContext.CurrentHash.I32[2] := SwapDWord(AContext.CurrentHash.I32[2]);
  AContext.CurrentHash.I32[3] := SwapDWord(AContext.CurrentHash.I32[3]);
  AContext.CurrentHash.I32[4] := SwapDWord(AContext.CurrentHash.I32[4]);
  AContext.CurrentHash.I32[5] := SwapDWord(AContext.CurrentHash.I32[5]);
  AContext.CurrentHash.I32[6] := SwapDWord(AContext.CurrentHash.I32[6]);
  AContext.CurrentHash.I32[7] := SwapDWord(AContext.CurrentHash.I32[7]);
  end;

  procedure SHA384Final;
  begin
  AContext.HashBuffer[AContext.Index] := $80;
  if AContext.Index >= 112 then
    SHACompress(AContext);
  PInt64(@AContext.HashBuffer[112])^ := SwapDWord(AContext.LenHi);
  PInt64(@AContext.HashBuffer[120])^ := SwapDWord(AContext.LenLo);
  SHACompress(AContext);
  AContext.CurrentHash.I64[0] := SwapQWord(AContext.CurrentHash.I64[0]);
  AContext.CurrentHash.I64[1] := SwapQWord(AContext.CurrentHash.I64[1]);
  AContext.CurrentHash.I64[2] := SwapQWord(AContext.CurrentHash.I64[2]);
  AContext.CurrentHash.I64[3] := SwapQWord(AContext.CurrentHash.I64[3]);
  AContext.CurrentHash.I64[4] := SwapQWord(AContext.CurrentHash.I64[4]);
  AContext.CurrentHash.I64[5] := SwapQWord(AContext.CurrentHash.I64[5]);
  end;
  procedure SHA512Final;
  begin
  AContext.HashBuffer[AContext.Index]:= $80;
  if AContext.Index>= 112 then
    SHACompress(AContext);
  PInt64(@AContext.HashBuffer[112])^:= SwapDWord(AContext.LenHi);
  PInt64(@AContext.HashBuffer[120])^:= SwapDWord(AContext.LenLo);
  SHACompress(AContext);
  AContext.CurrentHash.I64[0]:= SwapQWord(AContext.CurrentHash.I64[0]);
  AContext.CurrentHash.I64[1]:= SwapQWord(AContext.CurrentHash.I64[1]);
  AContext.CurrentHash.I64[2]:= SwapQWord(AContext.CurrentHash.I64[2]);
  AContext.CurrentHash.I64[3]:= SwapQWord(AContext.CurrentHash.I64[3]);
  AContext.CurrentHash.I64[4]:= SwapQWord(AContext.CurrentHash.I64[4]);
  AContext.CurrentHash.I64[5]:= SwapQWord(AContext.CurrentHash.I64[5]);
  AContext.CurrentHash.I64[6]:= SwapQWord(AContext.CurrentHash.I64[6]);
  AContext.CurrentHash.I64[7]:= SwapQWord(AContext.CurrentHash.I64[7]);
  end;

begin
case AContext.CurrentHash.HashType of
  sdt160:
    SHA160Final;
  sdt256:
    SHA256Final;
  sdt384:
    SHA384Final;
  sdt512:
    SHA512Final;
end;
end;

procedure SHAHash(var ADigest:TQSHADigest;const p: Pointer; len: Integer;AType:TQSHADigestType);inline;
var
  AContext:TQSHAContext;
begin
SHAInit(AContext,AType);
SHAUpdate(AContext,p,len);
SHAFinal(AContext);
ADigest:=AContext.CurrentHash;
end;

function SHA160Hash(const p: Pointer; len: Integer): TQSHADigest;
begin
SHAHash(Result,p,len,sdt160);
end;

procedure SHAHashString(const S:QStringW;AType:TQSHADigestType;var ADigest:TQSHADigest);
var
  A:QStringA;
begin
A:=QString.Utf8Encode(S);
SHAHash(ADigest,PQCharA(A),A.Length,AType);
end;

function SHAHashStream(AStream:TStream;AType:TQSHADigestType;var ADigest:TQSHADigest):TQSHADigest;
var
  AContext:TQSHAContext;
  ABuf:array[0..65535] of Byte;
  AReaded:Integer;
begin
SHAInit(AContext,AType);
AStream.Position:=0;
repeat
  AReaded:=AStream.Read(ABuf,65536);
  if AReaded>0 then
    SHAUpdate(AContext,@ABuf[0],AReaded);
until AReaded=0;
SHAFinal(AContext);
Result:=AContext.CurrentHash;
end;

procedure SHAHashFile(AFile:QStringW;AType:TQSHADigestType;var ADigest:TQSHADigest);
var
  AStream:TFileStream;
begin
AStream:=TFileStream.Create(AFile,fmOpenRead or fmShareDenyWrite);
try
  SHAHashStream(AStream,AType,ADigest);
finally
  FreeObject(AStream);
end;
end;

function SHA160Hash(const S:QStringW): TQSHADigest;
begin
SHAHashString(S,sdt160,Result);
end;

function SHA160Hash(AStream:TStream):TQSHADigest;
begin
SHAHashStream(AStream,sdt160,Result);
end;
function SHA160File(AFileName:QStringW):TQSHADigest;
begin
SHAHashFile(AFileName,sdt160,Result);
end;

function SHA256Hash(const p: Pointer; len: Integer): TQSHADigest;
begin
SHAHash(Result,p,len,sdt256);
end;

function SHA256Hash(const S:QStringW): TQSHADigest;
begin
SHAHashString(S,sdt256,Result);
end;

function SHA256Hash(AStream:TStream):TQSHADigest;
begin
SHAHashStream(AStream,sdt256,Result);
end;

function SHA256File(AFileName:QStringW):TQSHADigest;
begin
SHAHashFile(AFileName,sdt256,Result);
end;

function SHA384Hash(const p: Pointer; len: Integer): TQSHADigest;
begin
SHAHash(Result,p,len,sdt384);
end;

function SHA384Hash(const S:QStringW): TQSHADigest;
begin
SHAHashString(S,sdt384,Result);
end;

function SHA384Hash(AStream:TStream):TQSHADigest;
begin
SHAHashStream(AStream,sdt384,Result);
end;

function SHA384File(AFileName:QStringW):TQSHADigest;
begin
SHAHashFile(AFileName,sdt384,Result);
end;

function SHA512Hash(const p: Pointer; len: Integer): TQSHADigest;
begin
SHAHash(Result,p,len,sdt512);
end;

function SHA512Hash(const S:QStringW): TQSHADigest;
begin
SHAHashString(S,sdt512,Result);
end;

function SHA512Hash(AStream:TStream):TQSHADigest;
begin
SHAHashStream(AStream,sdt512,Result);
end;

function SHA512File(AFileName:QStringW):TQSHADigest;
begin
SHAHashFile(AFileName,sdt512,Result);
end;

function DigestToString(const ADigest:TQSHADigest):QStringW;
begin
case ADigest.HashType of
  sdt160:
    Result:=QString.BinToHex(@ADigest.SHA160[0],20);
  sdt256:
    Result:=QString.BinToHex(@ADigest.SHA256[0],32);
  sdt384:
    Result:=QString.BinToHex(@ADigest.SHA256[0],48);
  sdt512:
    Result:=QString.BinToHex(@ADigest.SHA256[0],64)
  else
    SetLength(Result,0);
end;
end;
end.

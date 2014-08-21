unit qaes;

interface

{ 本单元基于互联网上的AES加密实现稍加改动而来，其文件原作者暂不可知，先鸣谢先！
  AES加密是一种对称加密，理论上密文与原文长度一致，但由于AES是基于块加密的，所以
  产生的密文长度实际上是16的整数倍，不足的部分被填充为#0。所以如果要准确还原加密
  前的数据，你在保存时，应保存下其原始长度，以便解密后，对长度进行截断。
  关于CBC和EBC
  EBC是最原始的加密方法，CBC引入了初始化向量来与原始数据先异或再执行EBC相同的加密
  操作，理论上CBC更安全一点点。
}
uses classes, sysutils, qstring;

type
  TQAESKeyType = (kt128, kt192, kt256);
  TQAESEncryptMode = (emCBC, emECB);
  TQAESBuffer = array [0 .. 15] of byte;
  PQAESBuffer = ^TQAESBuffer;
  PQAES = ^TQAES;

  TQAES = record
  private
    FKey: QStringW;
    FMode: TQAESEncryptMode;
    FKeyType: TQAESKeyType;
    FInitVector: TQAESBuffer;
  public
    function AsECB(const AKey: QStringW;
      AKeyType: TQAESKeyType = kt256):PQAES; overload;
    function AsCBC(const AInitVector: TQAESBuffer; const AKey: QStringW;
      AKeyType: TQAESKeyType = kt256):PQAES; overload;
    procedure Encrypt(ASource, ADest: TStream); overload;
    procedure Encrypt(const p: Pointer; len: Integer;
      var AResult: TBytes); overload;
    procedure Encrypt(const AData: TBytes; var AResult: TBytes); overload;
    procedure Encrypt(const AData: QStringW; var AResult: TBytes); overload;
    procedure Encrypt(const ASourceFile, ADestFile: QStringW); overload;
    // 解密函数
    procedure Decrypt(ASource, ADest: TStream); overload;
    procedure Decrypt(const AData: TBytes; var AResult: TBytes); overload;
    function Decrypt(const AData: TBytes): String; overload;
    procedure Decrypt(const ASourceFile, ADestFile: QStringW); overload;
    property Key: QStringW read FKey write FKey;
    property Mode: TQAESEncryptMode read FMode;
    property KeyType: TQAESKeyType read FKeyType write FKeyType;
  end;

  // 计算加密后的内容大小，由于AES属于对称加密算法，但约定尺寸必是16的整数倍
  // 返回结果为((ASize/16)+1)*16
function AESEncryptSize(ASize: Int64): Int64;
// 加密函数,AInitVector(初始化向量)只是用于CBC模式，如果是ECB模式，它被忽略，
// 直接传AESEmptyBuffer就可以。在CBC模式下，加密和解决的初始化向量必需一致
procedure AESEncrypt(ASource, ADest: TStream; const AInitVector: TQAESBuffer;
  const AKey: QStringW; AKeyType: TQAESKeyType = kt256;
  AMode: TQAESEncryptMode = emCBC); overload;
procedure AESEncrypt(const p: Pointer; len: Integer;
  const AInitVector: TQAESBuffer; var AResult: TBytes; const AKey: QStringW;
  AKeyType: TQAESKeyType = kt256; AMode: TQAESEncryptMode = emCBC); overload;
procedure AESEncrypt(const AData: TBytes; const AInitVector: TQAESBuffer;
  var AResult: TBytes; const AKey: QStringW; AKeyType: TQAESKeyType = kt256;
  AMode: TQAESEncryptMode = emCBC); overload;
procedure AESEncrypt(const AData: QStringW; const AInitVector: TQAESBuffer;
  var AResult: TBytes; const AKey: QStringW; AKeyType: TQAESKeyType = kt256;
  AMode: TQAESEncryptMode = emCBC); overload;
procedure AESEncrypt(const ASourceFile, ADestFile: QStringW;
  const AInitVector: TQAESBuffer; const AKey: QStringW;
  AKeyType: TQAESKeyType = kt256; AMode: TQAESEncryptMode = emCBC); overload;
// 解密函数
procedure AESDecrypt(ASource, ADest: TStream; const AInitVector: TQAESBuffer;
  const AKey: QStringW; AKeyType: TQAESKeyType = kt256;
  AMode: TQAESEncryptMode = emCBC); overload;
procedure AESDecrypt(const AData: TBytes; const AInitVector: TQAESBuffer;
  var AResult: TBytes; const AKey: QStringW; AKeyType: TQAESKeyType = kt256;
  AMode: TQAESEncryptMode = emCBC); overload;
function AESDecrypt(const AData: TBytes; const AInitVector: TQAESBuffer;
  const AKey: QStringW; AKeyType: TQAESKeyType = kt256;
  AMode: TQAESEncryptMode = emCBC): String; overload;
procedure AESDecrypt(const ASourceFile, ADestFile: QStringW;
  const AInitVector: TQAESBuffer; const AKey: QStringW;
  AKeyType: TQAESKeyType = kt256; AMode: TQAESEncryptMode = emCBC); overload;

var
  AESEmptyBuffer: TQAESBuffer = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0);

implementation

uses math;

resourcestring
  SInvalidInBufSize = '密文流与加密时的流大小不匹配';
  SReadError = '无法读取数据，快到结尾？';
  SWriteError = '无法写入数据，磁盘已满？';

type
  EAESError = class(Exception);
  PInteger = ^Integer;
  PLongWord = ^Longword;

  TQAESKey128 = array [0 .. 15] of byte;
  TQAESKey192 = array [0 .. 23] of byte;
  TQAESKey256 = array [0 .. 31] of byte;
  TQAESExpandedKey128 = array [0 .. 43] of Longword;
  TQAESExpandedKey192 = array [0 .. 53] of Longword;
  TQAESExpandedKey256 = array [0 .. 63] of Longword;

  PAESKey128 = ^TQAESKey128;
  PAESKey192 = ^TQAESKey192;
  PAESKey256 = ^TQAESKey256;
  PAESExpandedKey128 = ^TQAESExpandedKey128;
  PAESExpandedKey192 = ^TQAESExpandedKey192;
  PAESExpandedKey256 = ^TQAESExpandedKey256;

  TQAESKey = record
    case Integer of
      0:
        (Key128: TQAESKey128);
      1:
        (Key192: TQAESKey192);
      2:
        (Key256: TQAESKey256);
  end;

  TQAESExpandedKey = record
    case Integer of
      0:
        (Key128: TQAESExpandedKey128);
      1:
        (Key192: TQAESExpandedKey192);
      2:
        (Key256: TQAESExpandedKey256);
  end;

const
  Rcon: array [1 .. 30] of Longword = ($00000001, $00000002, $00000004,
    $00000008, $00000010, $00000020, $00000040, $00000080, $0000001B, $00000036,
    $0000006C, $000000D8, $000000AB, $0000004D, $0000009A, $0000002F, $0000005E,
    $000000BC, $00000063, $000000C6, $00000097, $00000035, $0000006A, $000000D4,
    $000000B3, $0000007D, $000000FA, $000000EF, $000000C5, $00000091);

  ForwardTable: array [0 .. 255] of Longword = ($A56363C6, $847C7CF8, $997777EE,
    $8D7B7BF6, $0DF2F2FF, $BD6B6BD6, $B16F6FDE, $54C5C591, $50303060, $03010102,
    $A96767CE, $7D2B2B56, $19FEFEE7, $62D7D7B5, $E6ABAB4D, $9A7676EC, $45CACA8F,
    $9D82821F, $40C9C989, $877D7DFA, $15FAFAEF, $EB5959B2, $C947478E, $0BF0F0FB,
    $ECADAD41, $67D4D4B3, $FDA2A25F, $EAAFAF45, $BF9C9C23, $F7A4A453, $967272E4,
    $5BC0C09B, $C2B7B775, $1CFDFDE1, $AE93933D, $6A26264C, $5A36366C, $413F3F7E,
    $02F7F7F5, $4FCCCC83, $5C343468, $F4A5A551, $34E5E5D1, $08F1F1F9, $937171E2,
    $73D8D8AB, $53313162, $3F15152A, $0C040408, $52C7C795, $65232346, $5EC3C39D,
    $28181830, $A1969637, $0F05050A, $B59A9A2F, $0907070E, $36121224, $9B80801B,
    $3DE2E2DF, $26EBEBCD, $6927274E, $CDB2B27F, $9F7575EA, $1B090912, $9E83831D,
    $742C2C58, $2E1A1A34, $2D1B1B36, $B26E6EDC, $EE5A5AB4, $FBA0A05B, $F65252A4,
    $4D3B3B76, $61D6D6B7, $CEB3B37D, $7B292952, $3EE3E3DD, $712F2F5E, $97848413,
    $F55353A6, $68D1D1B9, $00000000, $2CEDEDC1, $60202040, $1FFCFCE3, $C8B1B179,
    $ED5B5BB6, $BE6A6AD4, $46CBCB8D, $D9BEBE67, $4B393972, $DE4A4A94, $D44C4C98,
    $E85858B0, $4ACFCF85, $6BD0D0BB, $2AEFEFC5, $E5AAAA4F, $16FBFBED, $C5434386,
    $D74D4D9A, $55333366, $94858511, $CF45458A, $10F9F9E9, $06020204, $817F7FFE,
    $F05050A0, $443C3C78, $BA9F9F25, $E3A8A84B, $F35151A2, $FEA3A35D, $C0404080,
    $8A8F8F05, $AD92923F, $BC9D9D21, $48383870, $04F5F5F1, $DFBCBC63, $C1B6B677,
    $75DADAAF, $63212142, $30101020, $1AFFFFE5, $0EF3F3FD, $6DD2D2BF, $4CCDCD81,
    $140C0C18, $35131326, $2FECECC3, $E15F5FBE, $A2979735, $CC444488, $3917172E,
    $57C4C493, $F2A7A755, $827E7EFC, $473D3D7A, $AC6464C8, $E75D5DBA, $2B191932,
    $957373E6, $A06060C0, $98818119, $D14F4F9E, $7FDCDCA3, $66222244, $7E2A2A54,
    $AB90903B, $8388880B, $CA46468C, $29EEEEC7, $D3B8B86B, $3C141428, $79DEDEA7,
    $E25E5EBC, $1D0B0B16, $76DBDBAD, $3BE0E0DB, $56323264, $4E3A3A74, $1E0A0A14,
    $DB494992, $0A06060C, $6C242448, $E45C5CB8, $5DC2C29F, $6ED3D3BD, $EFACAC43,
    $A66262C4, $A8919139, $A4959531, $37E4E4D3, $8B7979F2, $32E7E7D5, $43C8C88B,
    $5937376E, $B76D6DDA, $8C8D8D01, $64D5D5B1, $D24E4E9C, $E0A9A949, $B46C6CD8,
    $FA5656AC, $07F4F4F3, $25EAEACF, $AF6565CA, $8E7A7AF4, $E9AEAE47, $18080810,
    $D5BABA6F, $887878F0, $6F25254A, $722E2E5C, $241C1C38, $F1A6A657, $C7B4B473,
    $51C6C697, $23E8E8CB, $7CDDDDA1, $9C7474E8, $211F1F3E, $DD4B4B96, $DCBDBD61,
    $868B8B0D, $858A8A0F, $907070E0, $423E3E7C, $C4B5B571, $AA6666CC, $D8484890,
    $05030306, $01F6F6F7, $120E0E1C, $A36161C2, $5F35356A, $F95757AE, $D0B9B969,
    $91868617, $58C1C199, $271D1D3A, $B99E9E27, $38E1E1D9, $13F8F8EB, $B398982B,
    $33111122, $BB6969D2, $70D9D9A9, $898E8E07, $A7949433, $B69B9B2D, $221E1E3C,
    $92878715, $20E9E9C9, $49CECE87, $FF5555AA, $78282850, $7ADFDFA5, $8F8C8C03,
    $F8A1A159, $80898909, $170D0D1A, $DABFBF65, $31E6E6D7, $C6424284, $B86868D0,
    $C3414182, $B0999929, $772D2D5A, $110F0F1E, $CBB0B07B, $FC5454A8, $D6BBBB6D,
    $3A16162C);

  LastForwardTable: array [0 .. 255] of Longword = ($00000063, $0000007C,
    $00000077, $0000007B, $000000F2, $0000006B, $0000006F, $000000C5, $00000030,
    $00000001, $00000067, $0000002B, $000000FE, $000000D7, $000000AB, $00000076,
    $000000CA, $00000082, $000000C9, $0000007D, $000000FA, $00000059, $00000047,
    $000000F0, $000000AD, $000000D4, $000000A2, $000000AF, $0000009C, $000000A4,
    $00000072, $000000C0, $000000B7, $000000FD, $00000093, $00000026, $00000036,
    $0000003F, $000000F7, $000000CC, $00000034, $000000A5, $000000E5, $000000F1,
    $00000071, $000000D8, $00000031, $00000015, $00000004, $000000C7, $00000023,
    $000000C3, $00000018, $00000096, $00000005, $0000009A, $00000007, $00000012,
    $00000080, $000000E2, $000000EB, $00000027, $000000B2, $00000075, $00000009,
    $00000083, $0000002C, $0000001A, $0000001B, $0000006E, $0000005A, $000000A0,
    $00000052, $0000003B, $000000D6, $000000B3, $00000029, $000000E3, $0000002F,
    $00000084, $00000053, $000000D1, $00000000, $000000ED, $00000020, $000000FC,
    $000000B1, $0000005B, $0000006A, $000000CB, $000000BE, $00000039, $0000004A,
    $0000004C, $00000058, $000000CF, $000000D0, $000000EF, $000000AA, $000000FB,
    $00000043, $0000004D, $00000033, $00000085, $00000045, $000000F9, $00000002,
    $0000007F, $00000050, $0000003C, $0000009F, $000000A8, $00000051, $000000A3,
    $00000040, $0000008F, $00000092, $0000009D, $00000038, $000000F5, $000000BC,
    $000000B6, $000000DA, $00000021, $00000010, $000000FF, $000000F3, $000000D2,
    $000000CD, $0000000C, $00000013, $000000EC, $0000005F, $00000097, $00000044,
    $00000017, $000000C4, $000000A7, $0000007E, $0000003D, $00000064, $0000005D,
    $00000019, $00000073, $00000060, $00000081, $0000004F, $000000DC, $00000022,
    $0000002A, $00000090, $00000088, $00000046, $000000EE, $000000B8, $00000014,
    $000000DE, $0000005E, $0000000B, $000000DB, $000000E0, $00000032, $0000003A,
    $0000000A, $00000049, $00000006, $00000024, $0000005C, $000000C2, $000000D3,
    $000000AC, $00000062, $00000091, $00000095, $000000E4, $00000079, $000000E7,
    $000000C8, $00000037, $0000006D, $0000008D, $000000D5, $0000004E, $000000A9,
    $0000006C, $00000056, $000000F4, $000000EA, $00000065, $0000007A, $000000AE,
    $00000008, $000000BA, $00000078, $00000025, $0000002E, $0000001C, $000000A6,
    $000000B4, $000000C6, $000000E8, $000000DD, $00000074, $0000001F, $0000004B,
    $000000BD, $0000008B, $0000008A, $00000070, $0000003E, $000000B5, $00000066,
    $00000048, $00000003, $000000F6, $0000000E, $00000061, $00000035, $00000057,
    $000000B9, $00000086, $000000C1, $0000001D, $0000009E, $000000E1, $000000F8,
    $00000098, $00000011, $00000069, $000000D9, $0000008E, $00000094, $0000009B,
    $0000001E, $00000087, $000000E9, $000000CE, $00000055, $00000028, $000000DF,
    $0000008C, $000000A1, $00000089, $0000000D, $000000BF, $000000E6, $00000042,
    $00000068, $00000041, $00000099, $0000002D, $0000000F, $000000B0, $00000054,
    $000000BB, $00000016);

  InverseTable: array [0 .. 255] of Longword = ($50A7F451, $5365417E, $C3A4171A,
    $965E273A, $CB6BAB3B, $F1459D1F, $AB58FAAC, $9303E34B, $55FA3020, $F66D76AD,
    $9176CC88, $254C02F5, $FCD7E54F, $D7CB2AC5, $80443526, $8FA362B5, $495AB1DE,
    $671BBA25, $980EEA45, $E1C0FE5D, $02752FC3, $12F04C81, $A397468D, $C6F9D36B,
    $E75F8F03, $959C9215, $EB7A6DBF, $DA595295, $2D83BED4, $D3217458, $2969E049,
    $44C8C98E, $6A89C275, $78798EF4, $6B3E5899, $DD71B927, $B64FE1BE, $17AD88F0,
    $66AC20C9, $B43ACE7D, $184ADF63, $82311AE5, $60335197, $457F5362, $E07764B1,
    $84AE6BBB, $1CA081FE, $942B08F9, $58684870, $19FD458F, $876CDE94, $B7F87B52,
    $23D373AB, $E2024B72, $578F1FE3, $2AAB5566, $0728EBB2, $03C2B52F, $9A7BC586,
    $A50837D3, $F2872830, $B2A5BF23, $BA6A0302, $5C8216ED, $2B1CCF8A, $92B479A7,
    $F0F207F3, $A1E2694E, $CDF4DA65, $D5BE0506, $1F6234D1, $8AFEA6C4, $9D532E34,
    $A055F3A2, $32E18A05, $75EBF6A4, $39EC830B, $AAEF6040, $069F715E, $51106EBD,
    $F98A213E, $3D06DD96, $AE053EDD, $46BDE64D, $B58D5491, $055DC471, $6FD40604,
    $FF155060, $24FB9819, $97E9BDD6, $CC434089, $779ED967, $BD42E8B0, $888B8907,
    $385B19E7, $DBEEC879, $470A7CA1, $E90F427C, $C91E84F8, $00000000, $83868009,
    $48ED2B32, $AC70111E, $4E725A6C, $FBFF0EFD, $5638850F, $1ED5AE3D, $27392D36,
    $64D90F0A, $21A65C68, $D1545B9B, $3A2E3624, $B1670A0C, $0FE75793, $D296EEB4,
    $9E919B1B, $4FC5C080, $A220DC61, $694B775A, $161A121C, $0ABA93E2, $E52AA0C0,
    $43E0223C, $1D171B12, $0B0D090E, $ADC78BF2, $B9A8B62D, $C8A91E14, $8519F157,
    $4C0775AF, $BBDD99EE, $FD607FA3, $9F2601F7, $BCF5725C, $C53B6644, $347EFB5B,
    $7629438B, $DCC623CB, $68FCEDB6, $63F1E4B8, $CADC31D7, $10856342, $40229713,
    $2011C684, $7D244A85, $F83DBBD2, $1132F9AE, $6DA129C7, $4B2F9E1D, $F330B2DC,
    $EC52860D, $D0E3C177, $6C16B32B, $99B970A9, $FA489411, $2264E947, $C48CFCA8,
    $1A3FF0A0, $D82C7D56, $EF903322, $C74E4987, $C1D138D9, $FEA2CA8C, $360BD498,
    $CF81F5A6, $28DE7AA5, $268EB7DA, $A4BFAD3F, $E49D3A2C, $0D927850, $9BCC5F6A,
    $62467E54, $C2138DF6, $E8B8D890, $5EF7392E, $F5AFC382, $BE805D9F, $7C93D069,
    $A92DD56F, $B31225CF, $3B99ACC8, $A77D1810, $6E639CE8, $7BBB3BDB, $097826CD,
    $F418596E, $01B79AEC, $A89A4F83, $656E95E6, $7EE6FFAA, $08CFBC21, $E6E815EF,
    $D99BE7BA, $CE366F4A, $D4099FEA, $D67CB029, $AFB2A431, $31233F2A, $3094A5C6,
    $C066A235, $37BC4E74, $A6CA82FC, $B0D090E0, $15D8A733, $4A9804F1, $F7DAEC41,
    $0E50CD7F, $2FF69117, $8DD64D76, $4DB0EF43, $544DAACC, $DF0496E4, $E3B5D19E,
    $1B886A4C, $B81F2CC1, $7F516546, $04EA5E9D, $5D358C01, $737487FA, $2E410BFB,
    $5A1D67B3, $52D2DB92, $335610E9, $1347D66D, $8C61D79A, $7A0CA137, $8E14F859,
    $893C13EB, $EE27A9CE, $35C961B7, $EDE51CE1, $3CB1477A, $59DFD29C, $3F73F255,
    $79CE1418, $BF37C773, $EACDF753, $5BAAFD5F, $146F3DDF, $86DB4478, $81F3AFCA,
    $3EC468B9, $2C342438, $5F40A3C2, $72C31D16, $0C25E2BC, $8B493C28, $41950DFF,
    $7101A839, $DEB30C08, $9CE4B4D8, $90C15664, $6184CB7B, $70B632D5, $745C6C48,
    $4257B8D0);

  LastInverseTable: array [0 .. 255] of Longword = ($00000052, $00000009,
    $0000006A, $000000D5, $00000030, $00000036, $000000A5, $00000038, $000000BF,
    $00000040, $000000A3, $0000009E, $00000081, $000000F3, $000000D7, $000000FB,
    $0000007C, $000000E3, $00000039, $00000082, $0000009B, $0000002F, $000000FF,
    $00000087, $00000034, $0000008E, $00000043, $00000044, $000000C4, $000000DE,
    $000000E9, $000000CB, $00000054, $0000007B, $00000094, $00000032, $000000A6,
    $000000C2, $00000023, $0000003D, $000000EE, $0000004C, $00000095, $0000000B,
    $00000042, $000000FA, $000000C3, $0000004E, $00000008, $0000002E, $000000A1,
    $00000066, $00000028, $000000D9, $00000024, $000000B2, $00000076, $0000005B,
    $000000A2, $00000049, $0000006D, $0000008B, $000000D1, $00000025, $00000072,
    $000000F8, $000000F6, $00000064, $00000086, $00000068, $00000098, $00000016,
    $000000D4, $000000A4, $0000005C, $000000CC, $0000005D, $00000065, $000000B6,
    $00000092, $0000006C, $00000070, $00000048, $00000050, $000000FD, $000000ED,
    $000000B9, $000000DA, $0000005E, $00000015, $00000046, $00000057, $000000A7,
    $0000008D, $0000009D, $00000084, $00000090, $000000D8, $000000AB, $00000000,
    $0000008C, $000000BC, $000000D3, $0000000A, $000000F7, $000000E4, $00000058,
    $00000005, $000000B8, $000000B3, $00000045, $00000006, $000000D0, $0000002C,
    $0000001E, $0000008F, $000000CA, $0000003F, $0000000F, $00000002, $000000C1,
    $000000AF, $000000BD, $00000003, $00000001, $00000013, $0000008A, $0000006B,
    $0000003A, $00000091, $00000011, $00000041, $0000004F, $00000067, $000000DC,
    $000000EA, $00000097, $000000F2, $000000CF, $000000CE, $000000F0, $000000B4,
    $000000E6, $00000073, $00000096, $000000AC, $00000074, $00000022, $000000E7,
    $000000AD, $00000035, $00000085, $000000E2, $000000F9, $00000037, $000000E8,
    $0000001C, $00000075, $000000DF, $0000006E, $00000047, $000000F1, $0000001A,
    $00000071, $0000001D, $00000029, $000000C5, $00000089, $0000006F, $000000B7,
    $00000062, $0000000E, $000000AA, $00000018, $000000BE, $0000001B, $000000FC,
    $00000056, $0000003E, $0000004B, $000000C6, $000000D2, $00000079, $00000020,
    $0000009A, $000000DB, $000000C0, $000000FE, $00000078, $000000CD, $0000005A,
    $000000F4, $0000001F, $000000DD, $000000A8, $00000033, $00000088, $00000007,
    $000000C7, $00000031, $000000B1, $00000012, $00000010, $00000059, $00000027,
    $00000080, $000000EC, $0000005F, $00000060, $00000051, $0000007F, $000000A9,
    $00000019, $000000B5, $0000004A, $0000000D, $0000002D, $000000E5, $0000007A,
    $0000009F, $00000093, $000000C9, $0000009C, $000000EF, $000000A0, $000000E0,
    $0000003B, $0000004D, $000000AE, $0000002A, $000000F5, $000000B0, $000000C8,
    $000000EB, $000000BB, $0000003C, $00000083, $00000053, $00000099, $00000061,
    $00000017, $0000002B, $00000004, $0000007E, $000000BA, $00000077, $000000D6,
    $00000026, $000000E1, $00000069, $00000014, $00000063, $00000055, $00000021,
    $0000000C, $0000007D);

procedure ExpandAESKeyForEncryption(const Key: TQAESKey128;
  var ExpandedKey: TQAESExpandedKey128); overload;
var
  I, J: Integer;
  T: Longword;
  W0, W1, W2, W3: Longword;
begin
ExpandedKey[0] := PLongWord(@Key[0])^;
ExpandedKey[1] := PLongWord(@Key[4])^;
ExpandedKey[2] := PLongWord(@Key[8])^;
ExpandedKey[3] := PLongWord(@Key[12])^;
I := 0;
J := 1;
repeat
  T := (ExpandedKey[I + 3] shl 24) or (ExpandedKey[I + 3] shr 8);
  W0 := LastForwardTable[byte(T)];
  W1 := LastForwardTable[byte(T shr 8)];
  W2 := LastForwardTable[byte(T shr 16)];
  W3 := LastForwardTable[byte(T shr 24)];
  ExpandedKey[I + 4] := ExpandedKey[I] xor (W0 xor ((W1 shl 8) or (W1 shr 24))
    xor ((W2 shl 16) or (W2 shr 16)) xor ((W3 shl 24) or (W3 shr 8)))
    xor Rcon[J];
  Inc(J);
  ExpandedKey[I + 5] := ExpandedKey[I + 1] xor ExpandedKey[I + 4];
  ExpandedKey[I + 6] := ExpandedKey[I + 2] xor ExpandedKey[I + 5];
  ExpandedKey[I + 7] := ExpandedKey[I + 3] xor ExpandedKey[I + 6];
  Inc(I, 4);
until I >= 40;
end;

procedure ExpandAESKeyForEncryption(const Key: TQAESKey192;
  var ExpandedKey: TQAESExpandedKey192); overload;
var
  I, J: Integer;
  T: Longword;
  W0, W1, W2, W3: Longword;
begin
ExpandedKey[0] := PLongWord(@Key[0])^;
ExpandedKey[1] := PLongWord(@Key[4])^;
ExpandedKey[2] := PLongWord(@Key[8])^;
ExpandedKey[3] := PLongWord(@Key[12])^;
ExpandedKey[4] := PLongWord(@Key[16])^;
ExpandedKey[5] := PLongWord(@Key[20])^;
I := 0;
J := 1;
repeat
  T := (ExpandedKey[I + 5] shl 24) or (ExpandedKey[I + 5] shr 8);
  W0 := LastForwardTable[byte(T)];
  W1 := LastForwardTable[byte(T shr 8)];
  W2 := LastForwardTable[byte(T shr 16)];
  W3 := LastForwardTable[byte(T shr 24)];
  ExpandedKey[I + 6] := ExpandedKey[I] xor (W0 xor ((W1 shl 8) or (W1 shr 24))
    xor ((W2 shl 16) or (W2 shr 16)) xor ((W3 shl 24) or (W3 shr 8)))
    xor Rcon[J];
  Inc(J);
  ExpandedKey[I + 7] := ExpandedKey[I + 1] xor ExpandedKey[I + 6];
  ExpandedKey[I + 8] := ExpandedKey[I + 2] xor ExpandedKey[I + 7];
  ExpandedKey[I + 9] := ExpandedKey[I + 3] xor ExpandedKey[I + 8];
  ExpandedKey[I + 10] := ExpandedKey[I + 4] xor ExpandedKey[I + 9];
  ExpandedKey[I + 11] := ExpandedKey[I + 5] xor ExpandedKey[I + 10];
  Inc(I, 6);
until I >= 46;
end;

procedure ExpandAESKeyForEncryption(const Key: TQAESKey256;
  var ExpandedKey: TQAESExpandedKey256); overload;
var
  I, J: Integer;
  T: Longword;
  W0, W1, W2, W3: Longword;
begin
ExpandedKey[0] := PLongWord(@Key[0])^;
ExpandedKey[1] := PLongWord(@Key[4])^;
ExpandedKey[2] := PLongWord(@Key[8])^;
ExpandedKey[3] := PLongWord(@Key[12])^;
ExpandedKey[4] := PLongWord(@Key[16])^;
ExpandedKey[5] := PLongWord(@Key[20])^;
ExpandedKey[6] := PLongWord(@Key[24])^;
ExpandedKey[7] := PLongWord(@Key[28])^;
I := 0;
J := 1;
repeat
  T := (ExpandedKey[I + 7] shl 24) or (ExpandedKey[I + 7] shr 8);
  W0 := LastForwardTable[byte(T)];
  W1 := LastForwardTable[byte(T shr 8)];
  W2 := LastForwardTable[byte(T shr 16)];
  W3 := LastForwardTable[byte(T shr 24)];
  ExpandedKey[I + 8] := ExpandedKey[I] xor (W0 xor ((W1 shl 8) or (W1 shr 24))
    xor ((W2 shl 16) or (W2 shr 16)) xor ((W3 shl 24) or (W3 shr 8)))
    xor Rcon[J];
  Inc(J);
  ExpandedKey[I + 9] := ExpandedKey[I + 1] xor ExpandedKey[I + 8];
  ExpandedKey[I + 10] := ExpandedKey[I + 2] xor ExpandedKey[I + 9];
  ExpandedKey[I + 11] := ExpandedKey[I + 3] xor ExpandedKey[I + 10];
  W0 := LastForwardTable[byte(ExpandedKey[I + 11])];
  W1 := LastForwardTable[byte(ExpandedKey[I + 11] shr 8)];
  W2 := LastForwardTable[byte(ExpandedKey[I + 11] shr 16)];
  W3 := LastForwardTable[byte(ExpandedKey[I + 11] shr 24)];
  ExpandedKey[I + 12] := ExpandedKey[I + 4]
    xor (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8)));
  ExpandedKey[I + 13] := ExpandedKey[I + 5] xor ExpandedKey[I + 12];
  ExpandedKey[I + 14] := ExpandedKey[I + 6] xor ExpandedKey[I + 13];
  ExpandedKey[I + 15] := ExpandedKey[I + 7] xor ExpandedKey[I + 14];
  Inc(I, 8);
until I >= 52;
end;

procedure EncrypTQAES(const InBuf: TQAESBuffer; const Key: TQAESExpandedKey128;
  var OutBuf: TQAESBuffer); overload;
var
  T0, T1: array [0 .. 3] of Longword;
  W0, W1, W2, W3: Longword;
begin
// initializing
T0[0] := PLongWord(@InBuf[0])^ xor Key[0];
T0[1] := PLongWord(@InBuf[4])^ xor Key[1];
T0[2] := PLongWord(@InBuf[8])^ xor Key[2];
T0[3] := PLongWord(@InBuf[12])^ xor Key[3];
// performing transformation 9 times
// round 1
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
// round 2
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
// round 3
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
// round 4
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
// round 5
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
// round 6
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
// round 7
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
// round 8
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
// round 9
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
// last round of transformations
W0 := LastForwardTable[byte(T1[0])];
W1 := LastForwardTable[byte(T1[1] shr 8)];
W2 := LastForwardTable[byte(T1[2] shr 16)];
W3 := LastForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
W0 := LastForwardTable[byte(T1[1])];
W1 := LastForwardTable[byte(T1[2] shr 8)];
W2 := LastForwardTable[byte(T1[3] shr 16)];
W3 := LastForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
W0 := LastForwardTable[byte(T1[2])];
W1 := LastForwardTable[byte(T1[3] shr 8)];
W2 := LastForwardTable[byte(T1[0] shr 16)];
W3 := LastForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
W0 := LastForwardTable[byte(T1[3])];
W1 := LastForwardTable[byte(T1[0] shr 8)];
W2 := LastForwardTable[byte(T1[1] shr 16)];
W3 := LastForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
// finalizing
PLongWord(@OutBuf[0])^ := T0[0];
PLongWord(@OutBuf[4])^ := T0[1];
PLongWord(@OutBuf[8])^ := T0[2];
PLongWord(@OutBuf[12])^ := T0[3];
end;

procedure EncrypTQAES(const InBuf: TQAESBuffer; const Key: TQAESExpandedKey192;
  var OutBuf: TQAESBuffer); overload;
var
  T0, T1: array [0 .. 3] of Longword;
  W0, W1, W2, W3: Longword;
begin
// initializing
T0[0] := PLongWord(@InBuf[0])^ xor Key[0];
T0[1] := PLongWord(@InBuf[4])^ xor Key[1];
T0[2] := PLongWord(@InBuf[8])^ xor Key[2];
T0[3] := PLongWord(@InBuf[12])^ xor Key[3];
// performing transformation 11 times
// round 1
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
// round 2
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
// round 3
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
// round 4
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
// round 5
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
// round 6
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
// round 7
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
// round 8
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
// round 9
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
// round 10
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
// round 11
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[44];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[45];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[46];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[47];
// last round of transformations
W0 := LastForwardTable[byte(T1[0])];
W1 := LastForwardTable[byte(T1[1] shr 8)];
W2 := LastForwardTable[byte(T1[2] shr 16)];
W3 := LastForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[48];
W0 := LastForwardTable[byte(T1[1])];
W1 := LastForwardTable[byte(T1[2] shr 8)];
W2 := LastForwardTable[byte(T1[3] shr 16)];
W3 := LastForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[49];
W0 := LastForwardTable[byte(T1[2])];
W1 := LastForwardTable[byte(T1[3] shr 8)];
W2 := LastForwardTable[byte(T1[0] shr 16)];
W3 := LastForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[50];
W0 := LastForwardTable[byte(T1[3])];
W1 := LastForwardTable[byte(T1[0] shr 8)];
W2 := LastForwardTable[byte(T1[1] shr 16)];
W3 := LastForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[51];
// finalizing
PLongWord(@OutBuf[0])^ := T0[0];
PLongWord(@OutBuf[4])^ := T0[1];
PLongWord(@OutBuf[8])^ := T0[2];
PLongWord(@OutBuf[12])^ := T0[3];
end;

procedure EncrypTQAES(const InBuf: TQAESBuffer; const Key: TQAESExpandedKey256;
  var OutBuf: TQAESBuffer); overload;
var
  T0, T1: array [0 .. 3] of Longword;
  W0, W1, W2, W3: Longword;
begin
// initializing
T0[0] := PLongWord(@InBuf[0])^ xor Key[0];
T0[1] := PLongWord(@InBuf[4])^ xor Key[1];
T0[2] := PLongWord(@InBuf[8])^ xor Key[2];
T0[3] := PLongWord(@InBuf[12])^ xor Key[3];
// performing transformation 13 times
// round 1
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
// round 2
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
// round 3
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
// round 4
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
// round 5
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
// round 6
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
// round 7
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
// round 8
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
// round 9
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
// round 10
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
// round 11
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[44];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[45];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[46];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[47];
// round 12
W0 := ForwardTable[byte(T1[0])];
W1 := ForwardTable[byte(T1[1] shr 8)];
W2 := ForwardTable[byte(T1[2] shr 16)];
W3 := ForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[48];
W0 := ForwardTable[byte(T1[1])];
W1 := ForwardTable[byte(T1[2] shr 8)];
W2 := ForwardTable[byte(T1[3] shr 16)];
W3 := ForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[49];
W0 := ForwardTable[byte(T1[2])];
W1 := ForwardTable[byte(T1[3] shr 8)];
W2 := ForwardTable[byte(T1[0] shr 16)];
W3 := ForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[50];
W0 := ForwardTable[byte(T1[3])];
W1 := ForwardTable[byte(T1[0] shr 8)];
W2 := ForwardTable[byte(T1[1] shr 16)];
W3 := ForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[51];
// round 13
W0 := ForwardTable[byte(T0[0])];
W1 := ForwardTable[byte(T0[1] shr 8)];
W2 := ForwardTable[byte(T0[2] shr 16)];
W3 := ForwardTable[byte(T0[3] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[52];
W0 := ForwardTable[byte(T0[1])];
W1 := ForwardTable[byte(T0[2] shr 8)];
W2 := ForwardTable[byte(T0[3] shr 16)];
W3 := ForwardTable[byte(T0[0] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[53];
W0 := ForwardTable[byte(T0[2])];
W1 := ForwardTable[byte(T0[3] shr 8)];
W2 := ForwardTable[byte(T0[0] shr 16)];
W3 := ForwardTable[byte(T0[1] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[54];
W0 := ForwardTable[byte(T0[3])];
W1 := ForwardTable[byte(T0[0] shr 8)];
W2 := ForwardTable[byte(T0[1] shr 16)];
W3 := ForwardTable[byte(T0[2] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[55];
// last round of transformations
W0 := LastForwardTable[byte(T1[0])];
W1 := LastForwardTable[byte(T1[1] shr 8)];
W2 := LastForwardTable[byte(T1[2] shr 16)];
W3 := LastForwardTable[byte(T1[3] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[56];
W0 := LastForwardTable[byte(T1[1])];
W1 := LastForwardTable[byte(T1[2] shr 8)];
W2 := LastForwardTable[byte(T1[3] shr 16)];
W3 := LastForwardTable[byte(T1[0] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[57];
W0 := LastForwardTable[byte(T1[2])];
W1 := LastForwardTable[byte(T1[3] shr 8)];
W2 := LastForwardTable[byte(T1[0] shr 16)];
W3 := LastForwardTable[byte(T1[1] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[58];
W0 := LastForwardTable[byte(T1[3])];
W1 := LastForwardTable[byte(T1[0] shr 8)];
W2 := LastForwardTable[byte(T1[1] shr 16)];
W3 := LastForwardTable[byte(T1[2] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[59];
// finalizing
PLongWord(@OutBuf[0])^ := T0[0];
PLongWord(@OutBuf[4])^ := T0[1];
PLongWord(@OutBuf[8])^ := T0[2];
PLongWord(@OutBuf[12])^ := T0[3];
end;

procedure ExpandAESKeyForDecryption(var ExpandedKey
  : TQAESExpandedKey128); overload;
var
  I: Integer;
  U, F2, F4, F8, F9: Longword;
begin
for I := 1 to 9 do
  begin
  F9 := ExpandedKey[I * 4];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 1];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 1] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 2];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 2] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 3];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 3] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  end;
end;

procedure ExpandAESKeyForDecryption(const Key: TQAESKey128;
  var ExpandedKey: TQAESExpandedKey128); overload;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
ExpandAESKeyForDecryption(ExpandedKey);
end;

procedure ExpandAESKeyForDecryption(var ExpandedKey
  : TQAESExpandedKey192); overload;
var
  I: Integer;
  U, F2, F4, F8, F9: Longword;
begin
for I := 1 to 11 do
  begin
  F9 := ExpandedKey[I * 4];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 1];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 1] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 2];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 2] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 3];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 3] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  end;
end;

procedure ExpandAESKeyForDecryption(const Key: TQAESKey192;
  var ExpandedKey: TQAESExpandedKey192); overload;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
ExpandAESKeyForDecryption(ExpandedKey);
end;

procedure ExpandAESKeyForDecryption(var ExpandedKey
  : TQAESExpandedKey256); overload;
var
  I: Integer;
  U, F2, F4, F8, F9: Longword;
begin
for I := 1 to 13 do
  begin
  F9 := ExpandedKey[I * 4];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 1];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 1] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 2];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 2] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  F9 := ExpandedKey[I * 4 + 3];
  U := F9 and $80808080;
  F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F2 and $80808080;
  F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  U := F4 and $80808080;
  F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
  F9 := F9 xor F8;
  ExpandedKey[I * 4 + 3] := F2 xor F4 xor F8 xor (((F2 xor F9) shl 24) or
    ((F2 xor F9) shr 8)) xor (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16))
    xor ((F9 shl 8) or (F9 shr 24));
  end;
end;

procedure ExpandAESKeyForDecryption(const Key: TQAESKey256;
  var ExpandedKey: TQAESExpandedKey256); overload;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
ExpandAESKeyForDecryption(ExpandedKey);
end;

procedure DecrypTQAES(const InBuf: TQAESBuffer; const Key: TQAESExpandedKey128;
  var OutBuf: TQAESBuffer); overload;
var
  T0, T1: array [0 .. 3] of Longword;
  W0, W1, W2, W3: Longword;
begin
// initializing
T0[0] := PLongWord(@InBuf[0])^ xor Key[40];
T0[1] := PLongWord(@InBuf[4])^ xor Key[41];
T0[2] := PLongWord(@InBuf[8])^ xor Key[42];
T0[3] := PLongWord(@InBuf[12])^ xor Key[43];
// performing transformations 9 times
// round 1
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
// round 2
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
// round 3
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
// round 4
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
// round 5
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
// round 6
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
// round 7
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
// round 8
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
// round 9
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
// last round of transformations
W0 := LastInverseTable[byte(T1[0])];
W1 := LastInverseTable[byte(T1[3] shr 8)];
W2 := LastInverseTable[byte(T1[2] shr 16)];
W3 := LastInverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[0];
W0 := LastInverseTable[byte(T1[1])];
W1 := LastInverseTable[byte(T1[0] shr 8)];
W2 := LastInverseTable[byte(T1[3] shr 16)];
W3 := LastInverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[1];
W0 := LastInverseTable[byte(T1[2])];
W1 := LastInverseTable[byte(T1[1] shr 8)];
W2 := LastInverseTable[byte(T1[0] shr 16)];
W3 := LastInverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[2];
W0 := LastInverseTable[byte(T1[3])];
W1 := LastInverseTable[byte(T1[2] shr 8)];
W2 := LastInverseTable[byte(T1[1] shr 16)];
W3 := LastInverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[3];
// finalizing
PLongWord(@OutBuf[0])^ := T0[0];
PLongWord(@OutBuf[4])^ := T0[1];
PLongWord(@OutBuf[8])^ := T0[2];
PLongWord(@OutBuf[12])^ := T0[3];
end;

procedure DecrypTQAES(const InBuf: TQAESBuffer; const Key: TQAESExpandedKey192;
  var OutBuf: TQAESBuffer); overload;
var
  T0, T1: array [0 .. 3] of Longword;
  W0, W1, W2, W3: Longword;
begin
// initializing
T0[0] := PLongWord(@InBuf[0])^ xor Key[48];
T0[1] := PLongWord(@InBuf[4])^ xor Key[49];
T0[2] := PLongWord(@InBuf[8])^ xor Key[50];
T0[3] := PLongWord(@InBuf[12])^ xor Key[51];
// performing transformations 11 times
// round 1
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[44];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[45];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[46];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[47];
// round 2
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
// round 3
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
// round 4
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
// round 5
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
// round 6
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
// round 7
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
// round 8
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
// round 9
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
// round 10
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
// round 11
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
// last round of transformations
W0 := LastInverseTable[byte(T1[0])];
W1 := LastInverseTable[byte(T1[3] shr 8)];
W2 := LastInverseTable[byte(T1[2] shr 16)];
W3 := LastInverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[0];
W0 := LastInverseTable[byte(T1[1])];
W1 := LastInverseTable[byte(T1[0] shr 8)];
W2 := LastInverseTable[byte(T1[3] shr 16)];
W3 := LastInverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[1];
W0 := LastInverseTable[byte(T1[2])];
W1 := LastInverseTable[byte(T1[1] shr 8)];
W2 := LastInverseTable[byte(T1[0] shr 16)];
W3 := LastInverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[2];
W0 := LastInverseTable[byte(T1[3])];
W1 := LastInverseTable[byte(T1[2] shr 8)];
W2 := LastInverseTable[byte(T1[1] shr 16)];
W3 := LastInverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[3];
// finalizing
PLongWord(@OutBuf[0])^ := T0[0];
PLongWord(@OutBuf[4])^ := T0[1];
PLongWord(@OutBuf[8])^ := T0[2];
PLongWord(@OutBuf[12])^ := T0[3];
end;

procedure DecrypTQAES(const InBuf: TQAESBuffer; const Key: TQAESExpandedKey256;
  var OutBuf: TQAESBuffer); overload;
var
  T0, T1: array [0 .. 3] of Longword;
  W0, W1, W2, W3: Longword;
begin
// initializing
T0[0] := PLongWord(@InBuf[0])^ xor Key[56];
T0[1] := PLongWord(@InBuf[4])^ xor Key[57];
T0[2] := PLongWord(@InBuf[8])^ xor Key[58];
T0[3] := PLongWord(@InBuf[12])^ xor Key[59];
// performing transformations 13 times
// round 1
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[52];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[53];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[54];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[55];
// round 2
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[48];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[49];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[50];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[51];
// round 3
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[44];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[45];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[46];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[47];
// round 4
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
// round 5
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
// round 6
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
// round 7
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
// round 8
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
// round 9
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
// round 10
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
// round 11
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
// round 12
W0 := InverseTable[byte(T1[0])];
W1 := InverseTable[byte(T1[3] shr 8)];
W2 := InverseTable[byte(T1[2] shr 16)];
W3 := InverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
W0 := InverseTable[byte(T1[1])];
W1 := InverseTable[byte(T1[0] shr 8)];
W2 := InverseTable[byte(T1[3] shr 16)];
W3 := InverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
W0 := InverseTable[byte(T1[2])];
W1 := InverseTable[byte(T1[1] shr 8)];
W2 := InverseTable[byte(T1[0] shr 16)];
W3 := InverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
W0 := InverseTable[byte(T1[3])];
W1 := InverseTable[byte(T1[2] shr 8)];
W2 := InverseTable[byte(T1[1] shr 16)];
W3 := InverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
// round 13
W0 := InverseTable[byte(T0[0])];
W1 := InverseTable[byte(T0[3] shr 8)];
W2 := InverseTable[byte(T0[2] shr 16)];
W3 := InverseTable[byte(T0[1] shr 24)];
T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
W0 := InverseTable[byte(T0[1])];
W1 := InverseTable[byte(T0[0] shr 8)];
W2 := InverseTable[byte(T0[3] shr 16)];
W3 := InverseTable[byte(T0[2] shr 24)];
T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
W0 := InverseTable[byte(T0[2])];
W1 := InverseTable[byte(T0[1] shr 8)];
W2 := InverseTable[byte(T0[0] shr 16)];
W3 := InverseTable[byte(T0[3] shr 24)];
T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
W0 := InverseTable[byte(T0[3])];
W1 := InverseTable[byte(T0[2] shr 8)];
W2 := InverseTable[byte(T0[1] shr 16)];
W3 := InverseTable[byte(T0[0] shr 24)];
T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
// last round of transformations
W0 := LastInverseTable[byte(T1[0])];
W1 := LastInverseTable[byte(T1[3] shr 8)];
W2 := LastInverseTable[byte(T1[2] shr 16)];
W3 := LastInverseTable[byte(T1[1] shr 24)];
T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[0];
W0 := LastInverseTable[byte(T1[1])];
W1 := LastInverseTable[byte(T1[0] shr 8)];
W2 := LastInverseTable[byte(T1[3] shr 16)];
W3 := LastInverseTable[byte(T1[2] shr 24)];
T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[1];
W0 := LastInverseTable[byte(T1[2])];
W1 := LastInverseTable[byte(T1[1] shr 8)];
W2 := LastInverseTable[byte(T1[0] shr 16)];
W3 := LastInverseTable[byte(T1[3] shr 24)];
T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[2];
W0 := LastInverseTable[byte(T1[3])];
W1 := LastInverseTable[byte(T1[2] shr 8)];
W2 := LastInverseTable[byte(T1[1] shr 16)];
W3 := LastInverseTable[byte(T1[0] shr 24)];
T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
  xor ((W3 shl 24) or (W3 shr 8))) xor Key[3];
// finalizing
PLongWord(@OutBuf[0])^ := T0[0];
PLongWord(@OutBuf[4])^ := T0[1];
PLongWord(@OutBuf[8])^ := T0[2];
PLongWord(@OutBuf[12])^ := T0[3];
end;

procedure EncrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey128; Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Dec(Count, SizeOf(TQAESBuffer));
  end;
if Count > 0 then
  begin
  Done := Source.Read(TempIn, Count);
  if Done < Count then
    raise EStreamError.Create(SReadError);
  FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  end;
end;

procedure EncrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey192; Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Dec(Count, SizeOf(TQAESBuffer));
  end;
if Count > 0 then
  begin
  Done := Source.Read(TempIn, Count);
  if Done < Count then
    raise EStreamError.Create(SReadError);
  FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  end;
end;

procedure EncrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey256; Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Dec(Count, SizeOf(TQAESBuffer));
  end;
if Count > 0 then
  begin
  Done := Source.Read(TempIn, Count);
  if Done < Count then
    raise EStreamError.Create(SReadError);
  FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  end;
end;

procedure EncrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const Key: TQAESKey128; Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey128;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
EncrypTQAESStreamECB(Source, Count, ExpandedKey, Dest);
end;

procedure EncrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const Key: TQAESKey192; Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey192;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
EncrypTQAESStreamECB(Source, Count, ExpandedKey, Dest);
end;

procedure EncrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const Key: TQAESKey256; Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey256;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
EncrypTQAESStreamECB(Source, Count, ExpandedKey, Dest);
end;

procedure DecrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey128; Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
if (Count mod SizeOf(TQAESBuffer)) > 0 then
  raise EAESError.Create(SInvalidInBufSize);
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  DecrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Dec(Count, SizeOf(TQAESBuffer));
  end;
end;

procedure DecrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey192; Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
if (Count mod SizeOf(TQAESBuffer)) > 0 then
  raise EAESError.Create(SInvalidInBufSize);
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  DecrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Dec(Count, SizeOf(TQAESBuffer));
  end;
end;

procedure DecrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey256; Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
if (Count mod SizeOf(TQAESBuffer)) > 0 then
  raise EAESError.Create(SInvalidInBufSize);
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  DecrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Dec(Count, SizeOf(TQAESBuffer));
  end;
end;

procedure DecrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const Key: TQAESKey128; Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey128;
begin
ExpandAESKeyForDecryption(Key, ExpandedKey);
DecrypTQAESStreamECB(Source, Count, ExpandedKey, Dest);
end;

procedure DecrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const Key: TQAESKey192; Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey192;
begin
ExpandAESKeyForDecryption(Key, ExpandedKey);
DecrypTQAESStreamECB(Source, Count, ExpandedKey, Dest);
end;

procedure DecrypTQAESStreamECB(Source: TStream; Count: cardinal;
  const Key: TQAESKey256; Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey256;
begin
ExpandAESKeyForDecryption(Key, ExpandedKey);
DecrypTQAESStreamECB(Source, Count, ExpandedKey, Dest);
end;


// Stream encryption routines (CBC mode)

procedure EncrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey128; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  TempIn, TempOut, Vector: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
Vector := InitVector;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
  PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;
  PLongWord(@TempIn[8])^ := PLongWord(@TempIn[8])^ xor PLongWord(@Vector[8])^;
  PLongWord(@TempIn[12])^ := PLongWord(@TempIn[12])
    ^ xor PLongWord(@Vector[12])^;
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Vector := TempOut;
  Dec(Count, SizeOf(TQAESBuffer));
  end;
if Count > 0 then
  begin
  Done := Source.Read(TempIn, Count);
  if Done < Count then
    raise EStreamError.Create(SReadError);
  FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
  PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
  PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;
  PLongWord(@TempIn[8])^ := PLongWord(@TempIn[8])^ xor PLongWord(@Vector[8])^;
  PLongWord(@TempIn[12])^ := PLongWord(@TempIn[12])
    ^ xor PLongWord(@Vector[12])^;
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  end;
end;

procedure EncrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey192; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  TempIn, TempOut, Vector: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
Vector := InitVector;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
  PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;
  PLongWord(@TempIn[8])^ := PLongWord(@TempIn[8])^ xor PLongWord(@Vector[8])^;
  PLongWord(@TempIn[12])^ := PLongWord(@TempIn[12])
    ^ xor PLongWord(@Vector[12])^;
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Vector := TempOut;
  Dec(Count, SizeOf(TQAESBuffer));
  end;
if Count > 0 then
  begin
  Done := Source.Read(TempIn, Count);
  if Done < Count then
    raise EStreamError.Create(SReadError);
  FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
  PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
  PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;
  PLongWord(@TempIn[8])^ := PLongWord(@TempIn[8])^ xor PLongWord(@Vector[8])^;
  PLongWord(@TempIn[12])^ := PLongWord(@TempIn[12])
    ^ xor PLongWord(@Vector[12])^;
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  end;
end;

procedure EncrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey256; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  TempIn, TempOut, Vector: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
Vector := InitVector;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError.Create(SReadError);
  PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
  PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;
  PLongWord(@TempIn[8])^ := PLongWord(@TempIn[8])^ xor PLongWord(@Vector[8])^;
  PLongWord(@TempIn[12])^ := PLongWord(@TempIn[12])
    ^ xor PLongWord(@Vector[12])^;
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  Vector := TempOut;
  Dec(Count, SizeOf(TQAESBuffer));
  end;
if Count > 0 then
  begin
  Done := Source.Read(TempIn, Count);
  if Done < Count then
    raise EStreamError.Create(SReadError);
  FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
  PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
  PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;
  PLongWord(@TempIn[8])^ := PLongWord(@TempIn[8])^ xor PLongWord(@Vector[8])^;
  PLongWord(@TempIn[12])^ := PLongWord(@TempIn[12])
    ^ xor PLongWord(@Vector[12])^;
  EncrypTQAES(TempIn, ExpandedKey, TempOut);
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError.Create(SWriteError);
  end;
end;

procedure EncrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TQAESKey128; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey128;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
EncrypTQAESStreamCBC(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TQAESKey192; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey192;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
EncrypTQAESStreamCBC(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TQAESKey256; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey256;
begin
ExpandAESKeyForEncryption(Key, ExpandedKey);
EncrypTQAESStreamCBC(Source, Count, ExpandedKey, InitVector, Dest);
end;

// Stream decryption routines (CBC mode)

procedure DecrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey128; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Vector1, Vector2: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
if (Count mod SizeOf(TQAESBuffer)) > 0 then
  raise EAESError.Create(SInvalidInBufSize);
Vector1 := InitVector;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError(SReadError);
  Vector2 := TempIn;
  DecrypTQAES(TempIn, ExpandedKey, TempOut);
  PLongWord(@TempOut[0])^ := PLongWord(@TempOut[0])
    ^ xor PLongWord(@Vector1[0])^;
  PLongWord(@TempOut[4])^ := PLongWord(@TempOut[4])
    ^ xor PLongWord(@Vector1[4])^;
  PLongWord(@TempOut[8])^ := PLongWord(@TempOut[8])
    ^ xor PLongWord(@Vector1[8])^;
  PLongWord(@TempOut[12])^ := PLongWord(@TempOut[12])
    ^ xor PLongWord(@Vector1[12])^;
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError(SWriteError);
  Vector1 := Vector2;
  Dec(Count, SizeOf(TQAESBuffer));
  end;
end;

procedure DecrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey192; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Vector1, Vector2: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
if (Count mod SizeOf(TQAESBuffer)) > 0 then
  raise EAESError.Create(SInvalidInBufSize);
Vector1 := InitVector;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError(SReadError);
  Vector2 := TempIn;
  DecrypTQAES(TempIn, ExpandedKey, TempOut);
  PLongWord(@TempOut[0])^ := PLongWord(@TempOut[0])
    ^ xor PLongWord(@Vector1[0])^;
  PLongWord(@TempOut[4])^ := PLongWord(@TempOut[4])
    ^ xor PLongWord(@Vector1[4])^;
  PLongWord(@TempOut[8])^ := PLongWord(@TempOut[8])
    ^ xor PLongWord(@Vector1[8])^;
  PLongWord(@TempOut[12])^ := PLongWord(@TempOut[12])
    ^ xor PLongWord(@Vector1[12])^;
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError(SWriteError);
  Vector1 := Vector2;
  Dec(Count, SizeOf(TQAESBuffer));
  end;
end;

procedure DecrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TQAESExpandedKey256; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  TempIn, TempOut: TQAESBuffer;
  Vector1, Vector2: TQAESBuffer;
  Done: cardinal;
begin
if Count = 0 then
  begin
  Source.Position := 0;
  Count := Source.Size;
  end
else
  Count := Min(Count, Source.Size - Source.Position);
if Count = 0 then
  exit;
if (Count mod SizeOf(TQAESBuffer)) > 0 then
  raise EAESError.Create(SInvalidInBufSize);
Vector1 := InitVector;
while Count >= SizeOf(TQAESBuffer) do
  begin
  Done := Source.Read(TempIn, SizeOf(TempIn));
  if Done < SizeOf(TempIn) then
    raise EStreamError(SReadError);
  Vector2 := TempIn;
  DecrypTQAES(TempIn, ExpandedKey, TempOut);
  PLongWord(@TempOut[0])^ := PLongWord(@TempOut[0])
    ^ xor PLongWord(@Vector1[0])^;
  PLongWord(@TempOut[4])^ := PLongWord(@TempOut[4])
    ^ xor PLongWord(@Vector1[4])^;
  PLongWord(@TempOut[8])^ := PLongWord(@TempOut[8])
    ^ xor PLongWord(@Vector1[8])^;
  PLongWord(@TempOut[12])^ := PLongWord(@TempOut[12])
    ^ xor PLongWord(@Vector1[12])^;
  Done := Dest.Write(TempOut, SizeOf(TempOut));
  if Done < SizeOf(TempOut) then
    raise EStreamError(SWriteError);
  Vector1 := Vector2;
  Dec(Count, SizeOf(TQAESBuffer));
  end;
end;

procedure DecrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TQAESKey128; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey128;
begin
ExpandAESKeyForDecryption(Key, ExpandedKey);
DecrypTQAESStreamCBC(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TQAESKey192; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey192;
begin
ExpandAESKeyForDecryption(Key, ExpandedKey);
DecrypTQAESStreamCBC(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecrypTQAESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TQAESKey256; const InitVector: TQAESBuffer;
  Dest: TStream); overload;
var
  ExpandedKey: TQAESExpandedKey256;
begin
ExpandAESKeyForDecryption(Key, ExpandedKey);
DecrypTQAESStreamCBC(Source, Count, ExpandedKey, InitVector, Dest);
end;

function GenerateKey(const S: QStringW; AKeyType: TQAESKeyType): TQAESKey;
var
  U: QStringA;
  L: Integer;
begin
U := qstring.Utf8Encode(S);
L := U.Length;
case AKeyType of
  kt128:
    begin
    if L > SizeOf(TQAESKey128) then
      Move(PQCharA(U)^, Result.Key128, SizeOf(TQAESKey128))
    else
      begin
      Move(PQCharA(U)^, Result.Key128, L);
      FillChar(Result.Key128[L], SizeOf(TQAESKey128) - L, 0);
      end;
    end;
  kt192:
    begin
    if L > SizeOf(TQAESKey192) then
      Move(PQCharA(U)^, Result.Key192, SizeOf(TQAESKey128))
    else
      begin
      Move(PQCharA(U)^, Result.Key192, L);
      FillChar(Result.Key192[L], SizeOf(TQAESKey192) - L, 0);
      end;
    end;
  kt256:
    begin
    if L > SizeOf(TQAESKey256) then
      Move(PQCharA(U)^, Result.Key256, SizeOf(TQAESKey128))
    else
      begin
      Move(PQCharA(U)^, Result.Key256, L);
      FillChar(Result.Key256[L], SizeOf(TQAESKey256) - L, 0);
      end;
    end;
end;
end;

procedure AESEncrypt(ASource, ADest: TStream; const AInitVector: TQAESBuffer;
  const AKey: QStringW; AKeyType: TQAESKeyType; AMode: TQAESEncryptMode);
begin
case AKeyType of
  kt128:
    begin
    if AMode = emECB then
      EncrypTQAESStreamECB(ASource, MaxInt, GenerateKey(AKey, AKeyType)
        .Key128, ADest)
    else
      EncrypTQAESStreamCBC(ASource, MaxInt, GenerateKey(AKey, AKeyType).Key128,
        AInitVector, ADest);
    end;
  kt192:
    begin
    if AMode = emECB then
      EncrypTQAESStreamECB(ASource, MaxInt, GenerateKey(AKey, AKeyType)
        .Key192, ADest)
    else
      EncrypTQAESStreamCBC(ASource, MaxInt, GenerateKey(AKey, AKeyType).Key192,
        AInitVector, ADest);
    end;
  kt256:
    begin
    if AMode = emECB then
      EncrypTQAESStreamECB(ASource, MaxInt, GenerateKey(AKey, AKeyType)
        .Key256, ADest)
    else
      EncrypTQAESStreamCBC(ASource, MaxInt, GenerateKey(AKey, AKeyType).Key256,
        AInitVector, ADest);
    end;
end;
end;

procedure AESEncrypt(const p: Pointer; len: Integer;
  const AInitVector: TQAESBuffer; var AResult: TBytes; const AKey: QStringW;
  AKeyType: TQAESKeyType; AMode: TQAESEncryptMode); overload;
var
  pIn, pOut: PQAESBuffer;
  ABuf: TQAESBuffer; // 用来处理最后不足16字节的内容
  AKeyData: TQAESKey;
  AExpendedKey: TQAESExpandedKey;
  procedure Encrypt128EBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key128, AExpendedKey.Key128);
  while len > SizeOf(TQAESBuffer) do
    begin
    EncrypTQAES(pIn^, AExpendedKey.Key128, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  if len <> 0 then
    begin
    Move(pIn^, ABuf[0], len);
    FillChar(ABuf[len], SizeOf(TQAESBuffer) - len, 0);
    EncrypTQAES(ABuf, AExpendedKey.Key128, pOut^);
    end;
  end;

  procedure Encrypt192EBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key192, AExpendedKey.Key192);
  while len > SizeOf(TQAESBuffer) do
    begin
    EncrypTQAES(pIn^, AExpendedKey.Key192, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  if len <> 0 then
    begin
    Move(pIn^, ABuf[0], len);
    FillChar(ABuf[len], SizeOf(TQAESBuffer) - len, 0);
    EncrypTQAES(ABuf, AExpendedKey.Key192, pOut^);
    end;
  end;

  procedure Encrypt256EBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key256, AExpendedKey.Key256);
  while len > SizeOf(TQAESBuffer) do
    begin
    EncrypTQAES(pIn^, AExpendedKey.Key256, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  if len <> 0 then
    begin
    Move(pIn^, ABuf[0], len);
    FillChar(ABuf[len], SizeOf(TQAESBuffer) - len, 0);
    EncrypTQAES(ABuf, AExpendedKey.Key256, pOut^);
    end;
  end;

  procedure XorVector;
  begin
  PInt64(@ABuf[0])^ := PInt64(@ABuf[0])^ xor PInt64(@AInitVector[0])^;
  PInt64(@ABuf[8])^ := PInt64(@ABuf[8])^ xor PInt64(@AInitVector[8])^;
  end;

  procedure Encrypt128CBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key128, AExpendedKey.Key128);
  while len > SizeOf(TQAESBuffer) do
    begin
    ABuf := pIn^;
    XorVector;
    EncrypTQAES(ABuf, AExpendedKey.Key128, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  if len <> 0 then
    begin
    Move(pIn^, ABuf[0], len);
    FillChar(ABuf[len], SizeOf(TQAESBuffer) - len, 0);
    XorVector;
    EncrypTQAES(ABuf, AExpendedKey.Key128, pOut^);
    end;
  end;

  procedure Encrypt192CBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key192, AExpendedKey.Key192);
  while len > SizeOf(TQAESBuffer) do
    begin
    ABuf := pIn^;
    XorVector;
    EncrypTQAES(ABuf, AExpendedKey.Key192, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  if len <> 0 then
    begin
    Move(pIn^, ABuf[0], len);
    FillChar(ABuf[len], SizeOf(TQAESBuffer) - len, 0);
    XorVector;
    EncrypTQAES(ABuf, AExpendedKey.Key192, pOut^);
    end;
  end;

  procedure Encrypt256CBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key256, AExpendedKey.Key256);
  while len > SizeOf(TQAESBuffer) do
    begin
    ABuf := pIn^;
    XorVector;
    EncrypTQAES(ABuf, AExpendedKey.Key256, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  if len <> 0 then
    begin
    Move(pIn^, ABuf[0], len);
    FillChar(ABuf[len], SizeOf(TQAESBuffer) - len, 0);
    XorVector;
    EncrypTQAES(ABuf, AExpendedKey.Key256, pOut^);
    end;
  end;

begin
if len <= 0 then
  exit;
AKeyData := GenerateKey(AKey, AKeyType);
pIn := p;
if (len and $F) <> 0 then
  SetLength(AResult, ((len shr 4) + 1) shl 4)
else
  SetLength(AResult, len);
pOut := @AResult[0];
if AMode = emECB then
  begin
  case AKeyType of
    kt128:
      Encrypt128EBC;
    kt192:
      Encrypt192EBC;
    kt256:
      Encrypt256EBC;
  end;
  end
else // CBC
  begin
  case AKeyType of
    kt128:
      Encrypt128CBC;
    kt192:
      Encrypt192CBC;
    kt256:
      Encrypt256CBC;
  end;
  end;
end;

procedure AESEncrypt(const AData: TBytes; const AInitVector: TQAESBuffer;
  var AResult: TBytes; const AKey: QStringW; AKeyType: TQAESKeyType;
  AMode: TQAESEncryptMode);
begin
AESEncrypt(@AData[0], Length(AData), AInitVector, AResult, AKey,
  AKeyType, AMode);
end;

procedure AESEncrypt(const AData: QStringW; const AInitVector: TQAESBuffer;
  var AResult: TBytes; const AKey: QStringW; AKeyType: TQAESKeyType;
  AMode: TQAESEncryptMode);
var
  ATemp: QStringA;
begin
ATemp := qstring.Utf8Encode(AData);
AESEncrypt(PQCharA(ATemp), ATemp.Length, AInitVector, AResult, AKey,
  AKeyType, AMode);
end;

procedure AESEncrypt(const ASourceFile, ADestFile: QStringW;
  const AInitVector: TQAESBuffer; const AKey: QStringW; AKeyType: TQAESKeyType;
  AMode: TQAESEncryptMode);
var
  ASourceStream, ADestStream: TFileStream;
begin
ASourceStream := nil;
ADestStream := nil;
try
  ASourceStream := TFileStream.Create(ASourceFile, fmOpenRead or
    fmShareDenyWrite);
  ADestStream := TFileStream.Create(ADestFile, fmCreate);
  AESEncrypt(ASourceStream, ADestStream, AInitVector, AKey, AKeyType, AMode);
finally
  if ASourceStream <> nil then
    FreeObject(ASourceStream);
  if ADestStream <> nil then
    FreeObject(ADestStream);
end;
end;

procedure AESDecrypt(ASource, ADest: TStream; const AInitVector: TQAESBuffer;
  const AKey: QStringW; AKeyType: TQAESKeyType; AMode: TQAESEncryptMode);
begin
case AKeyType of
  kt128:
    begin
    if AMode = emECB then
      DecrypTQAESStreamECB(ASource, 0, GenerateKey(AKey, AKeyType)
        .Key128, ADest)
    else
      DecrypTQAESStreamCBC(ASource, 0, GenerateKey(AKey, AKeyType).Key128,
        AInitVector, ADest);
    end;
  kt192:
    begin
    if AMode = emECB then
      DecrypTQAESStreamECB(ASource, 0, GenerateKey(AKey, AKeyType)
        .Key192, ADest)
    else
      DecrypTQAESStreamCBC(ASource, 0, GenerateKey(AKey, AKeyType).Key192,
        AInitVector, ADest);
    end;
  kt256:
    begin
    if AMode = emECB then
      DecrypTQAESStreamECB(ASource, 0, GenerateKey(AKey, AKeyType)
        .Key256, ADest)
    else
      DecrypTQAESStreamCBC(ASource, 0, GenerateKey(AKey, AKeyType).Key256,
        AInitVector, ADest);
    end;
end;
end;

procedure AESDecrypt(const p: Pointer; len: Integer;
  const AInitVector: TQAESBuffer; var AResult: TBytes; const AKey: QStringW;
  AKeyType: TQAESKeyType; AMode: TQAESEncryptMode); overload;
var
  pIn, pOut: PQAESBuffer;
  AKeyData: TQAESKey;
  AExpendedKey: TQAESExpandedKey;

  procedure Decrypt128EBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key128, AExpendedKey.Key128);
  ExpandAESKeyForDecryption(AExpendedKey.Key128);
  while len > 0 do
    begin
    DecrypTQAES(pIn^, AExpendedKey.Key128, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  end;

  procedure Decrypt192EBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key192, AExpendedKey.Key192);
  ExpandAESKeyForDecryption(AExpendedKey.Key192);
  while len > 0 do
    begin
    DecrypTQAES(pIn^, AExpendedKey.Key192, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  end;

  procedure Decrypt256EBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key256, AExpendedKey.Key256);
  ExpandAESKeyForDecryption(AExpendedKey.Key256);
  while len > 0 do
    begin
    DecrypTQAES(pIn^, AExpendedKey.Key256, pOut^);
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  end;

  procedure XorVector;
  begin
  PInt64(IntPtr(pOut))^ := PInt64(IntPtr(pOut))^ xor PInt64(@AInitVector[0])^;
  PInt64(IntPtr(pOut) + 8)^ := PInt64(IntPtr(pOut) + 8)
    ^ xor PInt64(@AInitVector[8])^;
  end;

  procedure Decrypt128CBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key128, AExpendedKey.Key128);
  ExpandAESKeyForDecryption(AExpendedKey.Key128);
  while len > 0 do
    begin
    DecrypTQAES(pIn^, AExpendedKey.Key128, pOut^);
    XorVector;
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  end;

  procedure Decrypt192CBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key192, AExpendedKey.Key192);
  ExpandAESKeyForDecryption(AExpendedKey.Key192);
  while len > 0 do
    begin
    DecrypTQAES(pIn^, AExpendedKey.Key192, pOut^);
    XorVector;
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  end;

  procedure Decrypt256CBC;
  begin
  ExpandAESKeyForEncryption(AKeyData.Key256, AExpendedKey.Key256);
  ExpandAESKeyForDecryption(AExpendedKey.Key256);
  while len > 0 do
    begin
    DecrypTQAES(pIn^, AExpendedKey.Key256, pOut^);
    XorVector;
    Dec(len, SizeOf(TQAESBuffer));
    Inc(pIn);
    Inc(pOut);
    end;
  end;

begin
if len <= 0 then
  exit;
AKeyData := GenerateKey(AKey, AKeyType);
pIn := p;
if (len and $F) <> 0 then
  raise EAESError.Create(SInvalidInBufSize);
SetLength(AResult, len);
pOut := @AResult[0];
if AMode = emECB then
  begin
  case AKeyType of
    kt128:
      Decrypt128EBC;
    kt192:
      Decrypt192EBC;
    kt256:
      Decrypt256EBC;
  end;
  end
else // CBC
  begin
  case AKeyType of
    kt128:
      Decrypt128CBC;
    kt192:
      Decrypt192CBC;
    kt256:
      Decrypt256CBC;
  end;
  end;
end;

procedure AESDecrypt(const AData: TBytes; const AInitVector: TQAESBuffer;
  var AResult: TBytes; const AKey: QStringW; AKeyType: TQAESKeyType;
  AMode: TQAESEncryptMode);
begin
if Length(AData) > 0 then
  AESDecrypt(@AData[0], Length(AData), AInitVector, AResult, AKey,
    AKeyType, AMode)
else
  SetLength(AResult, 0);
end;

function AESDecrypt(const AData: TBytes; const AInitVector: TQAESBuffer;
  const AKey: QStringW; AKeyType: TQAESKeyType;
  AMode: TQAESEncryptMode): String;
var
  AResult: TBytes;
  L: Integer;
  ABOM: Boolean;
begin
AESDecrypt(AData, AInitVector, AResult, AKey, AKeyType, AMode);
L := Length(AResult);
if L > 0 then
  begin
  while L > 2 do
    begin
    if (AResult[L - 1] = 0) then
      Dec(L)
    else
      Break;
    end;
  case DetectTextEncoding(@AResult[0], L, ABOM) of
    teAnsi:
      Result := qstring.AnsiDecode(PQCharA(@AResult[0]), L);
    teUnicode16LE:
      if ABOM then
        Result := qstring.StrDupX(PQCharW(@AResult[1]), (L shr 1) - 1)
      else
        Result := qstring.StrDupX(PQCharW(@AResult[0]), L shr 1);
    teUnicode16BE:
      begin
      if ABOM then
        Result := qstring.StrDupX(PQCharW(@AResult[0]), (L shr 1) - 1)
      else
        Result := qstring.StrDupX(PQCharW(@AResult[0]), L shr 1);
      { Unicode BE 编码 }
      ExchangeByteOrder(PQCharA(Result), Length(Result) shl 1);
      end;
    teUTF8:
      { UTF8编码 }
      begin
      if ABOM then
        Result := qstring.Utf8Decode(PQCharA(@AResult[3]), L)
      else
        Result := qstring.Utf8Decode(PQCharA(@AResult[0]), L);
      end
  else
    SetLength(Result, 0);
  end;
  end
else
  SetLength(Result, 0);
end;

procedure AESDecrypt(const ASourceFile, ADestFile: QStringW;
  const AInitVector: TQAESBuffer; const AKey: QStringW; AKeyType: TQAESKeyType;
  AMode: TQAESEncryptMode);
var
  ASourceStream, ADestStream: TFileStream;
begin
ASourceStream := nil;
ADestStream := nil;
try
  ASourceStream := TFileStream.Create(ASourceFile, fmOpenRead or
    fmShareDenyWrite);
  ADestStream := TFileStream.Create(ADestFile, fmCreate);
  AESDecrypt(ASourceStream, ADestStream, AInitVector, AKey, AKeyType, AMode);
finally
  if ASourceStream <> nil then
    FreeObject(ASourceStream);
  if ADestStream <> nil then
    FreeObject(ADestStream);
end;
end;

function AESEncryptSize(ASize: Int64): Int64;
begin
if (ASize and $F) <> 0 then
  Result := ((ASize shr 4) + 1) shl 4
else
  Result := ASize;
end;

{ TQAES }

function TQAES.AsCBC(const AInitVector: TQAESBuffer; const AKey: QStringW;
  AKeyType: TQAESKeyType):PQAES;
begin
FKey := AKey;
FMode := emCBC;
FKeyType := AKeyType;
FInitVector := AInitVector;
Result:=@Self;
end;

function TQAES.AsECB(const AKey: QStringW; AKeyType: TQAESKeyType):PQAES;
begin
FKey := AKey;
FMode := emECB;
FKeyType := AKeyType;
FInitVector := AESEmptyBuffer;
Result:=@Self;
end;

procedure TQAES.Decrypt(ASource, ADest: TStream);
begin
AESDecrypt(ASource, ADest, FInitVector, FKey, FKeyType, FMode);
end;

procedure TQAES.Decrypt(const ASourceFile, ADestFile: QStringW);
begin
AESDecrypt(ASourceFile, ADestFile, FInitVector, FKey, FKeyType, FMode);
end;

function TQAES.Decrypt(const AData: TBytes): String;
begin
Result := AESDecrypt(AData, FInitVector, FKey, FKeyType, FMode);
end;

procedure TQAES.Decrypt(const AData: TBytes; var AResult: TBytes);
begin
AESDecrypt(AData, FInitVector, AResult, FKey, FKeyType, FMode);
end;

procedure TQAES.Encrypt(ASource, ADest: TStream);
begin
AESEncrypt(ASource, ADest, FInitVector, FKey, FKeyType, FMode);
end;

procedure TQAES.Encrypt(const p: Pointer; len: Integer; var AResult: TBytes);
begin
AESEncrypt(p, len, FInitVector, AResult, FKey, FKeyType, FMode);
end;

procedure TQAES.Encrypt(const ASourceFile, ADestFile: QStringW);
begin
AESEncrypt(ASourceFile, ADestFile, FInitVector, FKey, FKeyType, FMode);
end;

procedure TQAES.Encrypt(const AData: QStringW; var AResult: TBytes);
begin
AESEncrypt(AData, FInitVector, AResult, FKey, FKeyType, FMode);
end;

procedure TQAES.Encrypt(const AData: TBytes; var AResult: TBytes);
begin
AESEncrypt(AData, FInitVector, AResult, FKey, FKeyType, FMode);
end;

end.

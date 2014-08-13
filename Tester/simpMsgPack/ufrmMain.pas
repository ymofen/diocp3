unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, SimpleMsgPack, uByteTools;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    btn1: TButton;
    btnSimpMsgPack: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btnSimpMsgPackClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

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



{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  lvStream:TStream;
  w:Word;
  l :Cardinal;
begin
  w := $FFEE;
  Memo1.Lines.Add('===orignal');
  Memo1.Lines.Add(TByteTools.varToHexString(w, 2));
  Memo1.Lines.Add(IntToStr(w));

  lvStream := TMemoryStream.Create;
  w := swap16(w);
  lvStream.Write(w, SizeOf(w));

  lvStream.Position := 0;
  l := 0;
  lvStream.Read(l, 2);
  Memo1.Lines.Add(TByteTools.varToHexString(l, 4));
  Memo1.Lines.Add(TByteTools.varToHexString(word(l), 2));

  Memo1.Lines.Add(IntToStr(l));
  Memo1.Lines.Add(IntToStr(w));

  Memo1.Lines.Add('===restore');
  l := swap16(l);
  Memo1.Lines.Add(TByteTools.varToHexString(l, 4));
  Memo1.Lines.Add(IntToStr(l));

  l := $FFEE;
  Memo1.Lines.Add(TByteTools.varToHexString(l, 4));


end;

procedure TForm1.btnSimpMsgPackClick(Sender: TObject);
var
  lvMsg, lvMsg2:TSimpleMsgPack;
  lvBytes:TBytes;
begin
  lvMsg := TSimpleMsgPack.Create;
  lvMsg2 := TSimpleMsgPack.Create;
  //lvMsg.AsString := 'a龠bcde中国人民';
  lvMsg.AsString := '龠';
  lvBytes := lvMsg.EncodeToBytes;
  Memo1.Lines.Add(TByteTools.varToHexString(lvBytes[0], Length(lvBytes)));

  lvMsg2.DecodeFromBytes(lvBytes);
  Memo1.Lines.Add(lvMsg2.AsString);

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  w:Word;
  n2:SmallInt;

  n3:Cardinal;

  n4, n5:Int64;

begin

  w := $FFEE;
  n2 := $FF;
  Memo1.Lines.Add(TByteTools.varToHexString(w, 2));
  Memo1.Lines.Add(TByteTools.varToHexString(n2, SizeOf(SmallInt)));

  w := TByteTools.swap16(w);
  Memo1.Lines.Add(TByteTools.varToHexString(w, 2));

  n2 := TByteTools.swap16(n2);
  Memo1.Lines.Add(TByteTools.varToHexString(n2, SizeOf(SmallInt)));


  Memo1.Lines.Add('=========32=======');
  n3 := $FFEEDDCC;
  Memo1.Lines.Add(TByteTools.varToHexString(n3, SizeOf(n3)));
  n3 := swap32(n3);
  Memo1.Lines.Add(TByteTools.varToHexString(n3, SizeOf(n3)));
  n3 := TByteTools.swap32(n3);
  Memo1.Lines.Add(TByteTools.varToHexString(n3, SizeOf(n3)));

  Memo1.Lines.Add('=========64=======');
  n4 := $FFEEDDCCBBAA9988;
  Memo1.Lines.Add(TByteTools.varToHexString(n4, SizeOf(n4)));
  n5 := swap64(n4);
  Memo1.Lines.Add(TByteTools.varToHexString(n5, SizeOf(n5)));
  n5 := TByteTools.swap64(n4);
  Memo1.Lines.Add(TByteTools.varToHexString(n5, SizeOf(n5)));

end;

end.

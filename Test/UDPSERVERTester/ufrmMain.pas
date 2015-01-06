unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Un_LzUDP;

type
  TForm1 = class(TForm)
    edtListen: TEdit;
    btnListen: TButton;
    mmoRecv: TMemo;
    procedure btnListenClick(Sender: TObject);
  private
    { Private declarations }
    FUDPSocket: TLzUDPSocket;
    procedure OnDataRecv(UDPSocket: TLzUDPSocket; const PeerInfo: TPeerInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUDPSocket := TLzUDPSocket.Create;
  FUDPSocket.OnDataRead := OnDataRecv;
end;

procedure TForm1.OnDataRecv(UDPSocket: TLzUDPSocket; const PeerInfo: TPeerInfo);
var
  p: pointer;
  sz: integer;
begin
  GetMem(P, UDPSocket.RecvBufSize);
  try
    try
      sz := UDPSocket.RecvBuf(p^, UDPSocket.RecvBufSize, 0, 0);
      if sz > 0 then
      begin
         mmoRecv.Lines.Add(Format('接收到来自%d:%d的数据:%d', [PeerInfo.PeerIP, PeerInfo.PeerPort, sz]));
      end;
    except
      on e:Exception do
      begin
        mmoRecv.Lines.Add(e.Message);
      end;

    end;
  finally
    FreeMem(p);
  end;  

end;

procedure TForm1.btnListenClick(Sender: TObject);
begin
  FUDPSocket.Port := StrToInt(edtListen.Text);
  FUDPSocket.Active := true;
end;



destructor TForm1.Destroy;
begin
  FUDPSocket.Free;
  inherited Destroy;
end;

end.

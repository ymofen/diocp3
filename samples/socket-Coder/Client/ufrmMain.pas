unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpCoderTcpClient, iocpLogger;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    procedure btnConnectClick(Sender: TObject);
  private
    { Private declarations }
    FiocpCoderTcpClient:TiocpCoderTcpClient;

    procedure OnRecvObject(pvObject:TObject);
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JSonStream;



{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  uiLogger.setLogLines(mmoRecvMessage.Lines);
  FiocpCoderTcpClient := TiocpCoderTcpClient.Create(Self);
  FiocpCoderTcpClient.OnDataObjectReceived := OnRecvObject;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  FiocpCoderTcpClient.Host := edtHost.Text;
  FiocpCoderTcpClient.Port := StrToInt(edtPort.Text);
  FiocpCoderTcpClient.Connect;
end;

procedure TfrmMain.OnRecvObject(pvObject: TObject);
begin
  uiLogger.logMessage(TJsonStream(pvObject).Json.AsJSon(true, False));

end;

end.

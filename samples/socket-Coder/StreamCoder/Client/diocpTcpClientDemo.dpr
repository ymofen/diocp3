program diocpTcpClientDemo;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  FileTransProtocol in '..\Common\FileTransProtocol.pas',
  IocpFileASyncTrans in 'IocpFileASyncTrans.pas',
  uZipTools in '..\..\diocpCoders\uZipTools.pas',
  uDIOCPDxStreamCoder in '..\..\diocpCoders\uDIOCPDxStreamCoder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

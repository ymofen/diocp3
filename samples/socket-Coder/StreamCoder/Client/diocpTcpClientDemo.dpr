program diocpTcpClientDemo;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDIOCPStreamCoder in '..\..\diocpCoders\uDIOCPStreamCoder.pas',
  FileTransProtocol in '..\Common\FileTransProtocol.pas',
  IocpFileASyncTrans in 'IocpFileASyncTrans.pas',
  uZipTools in '..\..\diocpCoders\uZipTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

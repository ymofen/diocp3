(*

*)
unit udmMain;


interface

uses
  SysUtils, Classes, DB, ADODB;

type
  TdmMain = class(TDataModule)
    conMain: TADOConnection;
  public
    /// <summary>
    /// 客户端调用
    //     执行入口函数
    //  对vData所做的修改将会返回到客户端
    /// </summary>
    function Execute(pvCmdIndex: Integer; var vData: OleVariant): Boolean;
  end;

var
  dmMain: TdmMain;

implementation

{$R *.dfm}

function TdmMain.Execute(pvCmdIndex: Integer; var vData: OleVariant): Boolean;
begin
  
end;

end.

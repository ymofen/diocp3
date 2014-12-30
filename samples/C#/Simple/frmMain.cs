using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;


namespace DIOCPClient
{
    public partial class frmMain : Form
    {
        DAysncTcpClient diocpTcpClient = new DAysncTcpClient();
        public frmMain()
        {
            InitializeComponent();
            diocpTcpClient.OnConnected += this.OnConnected;
            diocpTcpClient.OnDisconnected += this.OnDisconnected;
            diocpTcpClient.OnRecvBuffer += this.OnRecvBuffer;
        }

        void OnRecvBuffer(byte[] buf, int len)
        {
            UTF8Encoding utf8 = new UTF8Encoding();

            String s = utf8.GetString(buf);
            this.Invoke(
                (MethodInvoker)delegate()
                {
                    this.txtRecv.Text = s;
                }
            );   
        }

        void OnDisconnected(object sender)
        {
            this.Invoke(
            (MethodInvoker)delegate()
            {
                this.Text = "已经断开连接";
            }
            );    
            
        }

        void OnConnected(object sender)
        {
            //System.Threading.Thread

            
            
            this.Invoke(
            (MethodInvoker)delegate()
                    {
                        this.Text = "已经连接";
                    }
            );            
           
        }

        private void button1_Click(object sender, EventArgs e)
        {
            diocpTcpClient.Host = txtHost.Text.Trim();
            diocpTcpClient.Port = int.Parse(txtPort.Text.Trim());
            diocpTcpClient.ConnectASync();
        }

        private void btnSend_Click(object sender, EventArgs e)
        {
            String s = txtSend.Text.Trim();
            UTF8Encoding utf8 = new UTF8Encoding();
            byte[] buf = utf8.GetBytes(s);
            diocpTcpClient.PostASyncBuffer(buf, buf.Length);
        }

    }
}

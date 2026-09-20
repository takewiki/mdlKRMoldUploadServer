
#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mdToolUploadServer()
mdToolUploadServer <- function(input,output,session,dms_token) {


  #获取参数
  text_file_mdTool_upload = tsui::var_file("text_file_mdTool_upload")



  shiny::observeEvent(input$btn_mdTool_upload,{

    filename =text_file_mdTool_upload()



    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      data <- readxl::read_excel(filename,col_types = c("numeric", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "date", "date"))

      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_src_t_mdTool_input',r_object = data,append = TRUE)


      mdlKRMoldUploadPkg::mdTool_upload(dms_token = dms_token)
      tsui::pop_notice("上传成功")
    }

  })

}

#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mdToolViewServer()
mdToolViewServer <- function(input,output,session,dms_token) {


  #获取参数
  text_file_mdTool_FBillNo = tsui::var_text("text_file_mdTool_FBillNo")



  shiny::observeEvent(input$btn_mdTool_view,{

    FBillNo =text_file_mdTool_FBillNo()

    data = mdlKRMoldUploadPkg::mdTool_view(dms_token = dms_token,FBillNo=FBillNo)

    tsui::run_dataTable2(id = 'mdTool_resultView',data = data)


    tsui::run_download_xlsx(id = 'dl_mdTool_download',data =data ,filename = '刀具.xlsx')


  })

}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mdToolServer()
mdToolServer <- function(input,output,session,dms_token) {

  mdToolUploadServer(input = input,output = output,session = session,dms_token = dms_token)
  mdToolViewServer(input = input,output = output,session = session,dms_token = dms_token)


}


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
#' mdMoldUploadServer()
mdMoldUploadServer <- function(input,output,session,dms_token) {


  #获取参数
  text_file_mdMold_upload = tsui::var_file("text_file_mdMold_upload")



  shiny::observeEvent(input$btn_mdMold_upload,{

    filename =text_file_mdMold_upload()



    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      data <- readxl::read_excel(filename,col_types = c("numeric", "text", "text",
                                                        "text", "text", "text", "numeric",
                                                        "text", "text", "date", "date"))


      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_src_t_mdMold_input',r_object = data,append = TRUE)


      mdlKRMoldUploadPkg::mdMold_upload(dms_token = dms_token)
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
#' mdMoldViewServer()
mdMoldViewServer <- function(input,output,session,dms_token) {


  #获取参数
  text_file_mdMold_FBillNo = tsui::var_text("text_file_mdMold_FBillNo")



  shiny::observeEvent(input$btn_mdMold_view,{

    FBillNo =text_file_mdMold_FBillNo()

    data = mdlKRMoldUploadPkg::mdMold_view(dms_token = dms_token,FBillNo=FBillNo)

    tsui::run_dataTable2(id = 'mdMold_resultView',data = data)


    tsui::run_download_xlsx(id = 'dl_mdMold_download',data =data ,filename = '模具.xlsx')


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
#' mdMoldServer()
mdMoldServer <- function(input,output,session,dms_token) {

  mdMoldUploadServer(input = input,output = output,session = session,dms_token = dms_token)
  mdMoldViewServer(input = input,output = output,session = session,dms_token = dms_token)


}

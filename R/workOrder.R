
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
#' workOrderUploadServer()
workOrderUploadServer <- function(input,output,session,dms_token) {


  #获取参数
  text_file_workOrder_upload = tsui::var_file("text_file_workOrder_upload")



  shiny::observeEvent(input$btn_workOrder_upload,{

    filename =text_file_workOrder_upload()



    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      data <- readxl::read_excel(filename,col_types = c("text", "text", "text", "text", "text","date",
                                                         "date", "date","text", "text", "text", "text",
                                                         "text", "text", "text", "text", "text", "text",
                                                         "text", "text", "text", "text", "text", "text",
                                                         "text", "text", "text","numeric","text", "text",
                                                         "text", "text", "text", "text","numeric", "numeric","text","date"))


      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_src_t_workOrder_input',r_object = data,append = TRUE)


      mdlKRMoldUploadPkg::workOrder_upload(dms_token = dms_token)
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
#' workOrderViewServer()
workOrderViewServer <- function(input,output,session,dms_token) {


  #获取参数
  text_file_workOrder_FBillNo = tsui::var_text("text_file_workOrder_FBillNo")



  shiny::observeEvent(input$btn_workOrder_view,{

    FBillNo =text_file_workOrder_FBillNo()

    data = mdlKRMoldUploadPkg::workOrder_view(dms_token = dms_token,FBillNo=FBillNo)

    tsui::run_dataTable2(id = 'workOrder_resultView',data = data)


    tsui::run_download_xlsx(id = 'dl_workOrder_download',data =data ,filename = '工单.xlsx')


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
#' workOrderServer()
workOrderServer <- function(input,output,session,dms_token) {

  workOrderUploadServer(input = input,output = output,session = session,dms_token = dms_token)
  workOrderViewServer(input = input,output = output,session = session,dms_token = dms_token)


}

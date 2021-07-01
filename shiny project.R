setwd("C:/Users/ASUS/Documents/MATERI KULIAH/KULIAH SEMESTER 4/SISTEM INFORMASI MANAJEMEN/Project Dashboard R Shiny")

library(plotrix)
library(shiny)
library(shinydashboard)
library(summarytools)
library(readxl)
library(DT)
library(plotly)
library(ggplot2)
library(rhandsontable)
library(datasets)
require(maps)  
require(mapdata) 
library(ggthemes) 
library(ggrepel)
library(highcharter)
library(ggcorrplot)

df1 <- read_excel("data_sim.xlsx", 
                  sheet = "demografi")
df2 <- read_excel("data_sim.xlsx", 
                  sheet = "penyakit")
df3 <- read_excel("data_sim.xlsx", 
                  sheet = "pengendalian")
df3 <- as.data.frame(df3)
df4 <- read_excel("data_sim.xlsx", sheet = "piramida")
df2.count <- read_excel("data_sim.xlsx", sheet = "Sheet1")
mydata <- read_excel("data_sim.xlsx", sheet = "demografi")

theme_lilik = function(){
  ggthemes::theme_tufte() +
    theme(plot.title = element_text(color="#184E77",size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 15,color="#184E77"),
          plot.caption = element_text(size = 5, color = '#184E77'),
          axis.title.x = element_text(size = 10, face = 'bold', vjust = 1,color="#184E77"),
          axis.text.x  = element_text(angle=45,size = 9,color="#184E77"),
          axis.title.y = element_text(size = 10, face = 'bold', vjust = 1,color="#184E77"),
          axis.text.y = element_text(size = 9,color="#184E77"),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = "#f0ead2"),
          panel.border = element_blank())
}



#header
headerItem<-dashboardHeader(title="KEMENKES RI",
                            dropdownMenu(type = "messages",
                                         messageItem(
                                           from = "Akun",
                                           message = "Gunakan akun yang terdaftar"
                                         ),
                                         messageItem(
                                           from = "Bantuan",
                                           message = "Hubungi kontak resmi",
                                           icon = icon("question"),
                                           time = "13:45"
                                         )
                            )
)

#sidebar
sidebarItem<-dashboardSidebar(width=280,
  sidebarMenu(
    menuItem("Penyusun", tabName = "penyusun",icon=icon("users")),
    menuItem("Pendahuluan", tabName = "pendahuluan",icon=icon("book")),
    menuItem("Data", tabName = "statdes", icon=icon("folder-open")),
    menuItem("Dashboard", tabName = "dashboard",icon=icon("dashboard"),
             menuSubItem("Demografi & Fasilitas Kesehatan ", tabName = "demografi"),
             menuSubItem("Penyakit & Kondisi Kesehatan ", tabName = "penyakit"),
             menuSubItem("Kebijakan Pengendalian Kesehatan", tabName = "kebijakan")),
    menuItem("Profi Kemenkes", tabName = "kontak",icon=icon("th"))
  )
)

#body
bodyItem=dashboardBody(
  tabItems(
    tabItem(tabName = "penyusun",
            h3("Data Diri Penyusun"),
            fluidPage(
              sidebarPanel(width = 6,height = 10,style="text-align:center",
                           tags$img(src ="www/lilik.png", height = 200, width = 150),
                           h5("Lilik Setyaningsih"),
                           h5("06211940000049"),
                           h5("liliksetyaningsih815@gmail.com")),
              sidebarPanel(width = 6,height = 10,style="text-align:center",
                           img(src ="foto_wardah.jpeg", height = 200, width = 150),
                           h5("Wahidatul Wardah Al Maulidiyah"),
                           h5("06211940000090"),
                           h5("wahidatul.wardah23@gmail.com")))
    ),
    tabItem(tabName = "pendahuluan",
            h3("Pendahuluan"),
            tabsetPanel(
              tabPanel(h4("Latar Belakang"),style="text-align:justify",h5("Kementerian Kesehatan (Kemenkes) merupakan salah satu kementerian yang memiliki peranan penting dalam kesehatan Indonesia, Kemenkes dibentuk dalam rangka membantu presiden untuk menyelenggarakan pemerintahan negara di bidang kesehatan. Pada tahun 2014 Kemenkes telah melaksanakan program kesehatan anak Indonesia, hal ini penting karena anak merupakan aset bagi bangsa dan negara. Selain itu, tercatat bahwasannya Kemenkes telah melaksanakan gerakan masyarakat hidup sehat ssuai dengan peraturan presiden no 1 tahun 2017. Kemenkes juga sangat berperan dalam rangka mewujudkan Visi Presiden Republik Indonesia tahun 2015-2019 yakni Terwujudnya Indonesia yang Berdaulat, Mandiri, dan Berkepribadian Berlandaskan Gotong royong, Kemenkes juga melaksanaan 7 misi pembangunan, khususnya misi ke-4 yakni mewujudkan kualitas hidup manusia Indonesia yang tinggi, maju, dan sejahtera. Kemenkes juga memiliki peran penting dalam rangka mencapai 9 agenda prioritas nasional yang dikenal dengan Nawacita, terutama agenda ke-S yakni meningkatkan kualitas hidup manusia Indonesia."),
                       h5("Namun sayangnya, banyak kementrian kesehatan yang ada di propinsi belum bisa mengembangkan potensinya secara maksimal, dikarenakan beberapa hal antara lain akses data, masih adanya ketimpangan tenaga kesehatan di masing masing propinsi, serta masih banyak lagi."),
                       h5("Hal tersebut tidak menjadikan penghalang bagi Kemenkes-Kemenkes yang berada di propinsi untuk terus berkontribusi bagi kesehatan di tanah air. Salah satu data yang memberikan andil besar dalam kesehatan di Indonesia yakni data profil kesehatan Indonesia. Akan tetapi data yang disajikan tidak menarik dikarenakan datanya hanya sebuah tabel dan isinya sangat banyak."),
                       h5("Berdasarkan permasalahan diatas, penulis berinovasi melalui Dashboard Profil Kesehatan Indonesia dengan tujuan untuk memberikan analisis lebih tajam dan mudah diterima masyarakat awam tentang kondisi kesehatan yang ada di Indonesia. Melalui dashboard ini, diharapkan Kementerian Kesehatan yang berada di provinsi-provinsi mampu memberikan visualisasi data yang mudah dipahami masyarakat awam.")
              ),
              tabPanel(h4("Tujuan"),h5("Adapun yang menjadi tujuan dari penelitian ini adalah :"),
                       h5("1. Merancang model pengembangan dashboard untuk kebutuhan Kementerian Kesehatan."),
                       h5("2. Menggambarkan profil Kesehatan Republik Indonesia."),
                       h5("3. Menampilkan visualisasi data variabel pada profil kesehatan Indonesia."),
                       h5("4. Memonitor kondisi kesehatan Indonesia.")
              ),
              tabPanel(h4("Manfaat"),h5("Adapun yang menjadi manfaat dari penelitian ini adalah :"),
                       h5("1. Sebagai media informasi yang dapat menyajikan informasi secara efisien dengan adanya grafik ataupun kalimat ringkasan dari informasi yang disajikan bagi seluruh pihak."),
                       h5("2. Sebagai media monitoring untuk dapat memantau progress atau perkembangan kesehatan Indonesia."),
                       h5("3. Sebagai dasar pengambilan keputusan bagi user."),
                       h5("4. Sebagai media analisis yang tepat.")
              )
            )),
    tabItem(tabName = "statdes",
            h3("Data Kementerian Kesehatan Indonesia Tahun 2019"),
            tabsetPanel(
              tabPanel("Data Frame",dataTableOutput("data_table")),
              tabPanel("Rangkuman 1",verbatimTextOutput("summary_stat")),
              tabPanel("Rangkuman 2",verbatimTextOutput("summary_stat1")),
              tabPanel("Keterangan", h4("Penjelasan variabel-variabel pada data adalah sebagai berikut :", face = "bold"),
                       h5("luas = luas wilayah"),
                       h5("utd1 = jumlah unit transfusi darah milik pemerintah"),
                       h5("utd2 = jumlah unit transfusi darah milik PMI"),
                       h5("utd3 = jumlah unit transfusi darah milik Kemenkes"),
                       h5("utd4 = jumlah unit transfusi darah milik swasta"),
                       h5("ds = jumlah dokter spesialis"),
                       h5("du = jumlah dokter umum"),
                       h5("dgs = jumlah dokter gigi spesialis"),
                       h5("dg = jumlah dokter gigi"),
                       h5("wus = jumlah wanita usia subur"),
                       h5("wusi = jumlah wanita usia subur imunisasi "),
                       h5("ih = jumlah ibu hamil"),
                       h5("ib = jumlah ibu bersalin"),
                       h5("pd = jumlah puskesmas yang terakreditasi dasar"),
                       h5("pm = jumlah puskesmas yang terakreditasi madya"),
                       h5("pu = jumlah puskesmas yang terakreditasi utama"),
                       h5("pp = jumlah puskesmas yang terakreditasi purna"),
                       h5("pb = jumlah puskesmas yang belum terakreditasi"),
                       h5("kpp = jumlah klinik pratama milik pemerintah"),
                       h5("kup = jumlah klinik utama milik pemerintah"),
                       h5("kpt = jumlah klinik pratama milik TNI"),
                       h5("kut = jumlah klinik utama milik TNI"),
                       h5("DBD = jumlah kasus DBD"),
                       h5("malaria = jumlah kasus malaria"),
                       h5("filariasis = jumlah kasus filariasis"),
                       h5("DM = jumlah kasus diabetes mellitus"),
                       h5("TBC = jumlah kasus TBC"),
                       h5("HIV = jumlah kasus HIV"),
                       h5("aids = jumlah kasus AIDS"),
                       h5("kusta = jumlah kasus kusta"),
                       h5("difteri = jumlah kasus difteri"),
                       h5("purtosis = jumlah kasus purtosis"),
                       h5("neonatorum = jumlah kasus neonatorum"),
                       h5("campak = jumlah kasus campak"),
                       h5("korban meninggal = jumlah korban meninggal akibat bencana"),
                       h5("luka berat = jumlah korban luka berat akibat bencana"),
                       h5("luka ringan = jumlah korban luka ringan akibat bencana"),
                       h5("hilang = jumlah korban hilang akibat bencana"),
                       h5("pengungsi = jumlah pengungsi akibat bencana"),
                       h5("% PVT = presentase keberhasilan program Pengendalian Vektor Terpadu"),
                       h5("% PTM = presentase keberhasilan program Penyakit Tidak Menular"),
                       h5("% POSBINDU = presentase keberhasilan program Pos Pembinaan Terpadu"),
                       h5("% PHBS = presentase keberhasilan program Perilaku Hidup Bersih dan Sehat"),
                       h5("% Kabupaten Sehat = presentase keberhasilan program Kabupaten Sehat"),
                       h5("% Air Sehat = presentase keberhasilan program Air Sehat"),
                       h5("% Jamban Sehat = presentase keberhasilan program Jamban Sehat"),
                       h5("% TU Sehat =  presentase keberhasilan program Tempat Umum Sehat"),
                       h5("% TPM Sehat = presentase keberhasilan program Tempat Pengelolaan Makanan Sehat")
              ))),
    tabItem(tabName = "demografi",h3("Demografi dan Fasilitas Kesehatan", face = "bold"),
            fluidPage(
              sidebarPanel(width = 4,height = 5,style="text-align:center",
                            selectInput("x2", "Variabel",
                                       choices = c("Wanita Usia Subur","Wanita Usia Subur Imunisasi", "Ibu Bersalin","Ibu Hamil"))),
              infoBoxOutput("penduduk",width = 4),
              infoBoxOutput("luas",width = 4)),
            fluidRow(box(title = "Kondisi Wanita Indonesia", plotlyOutput("plot2"),width = 12)),
            fluidPage(
              sidebarPanel(width = 12,height = 5,style="text-align:center",
                           selectInput("x1", "Variabel",
                                       choices = c("Unit Transfusi Darah Milik Pemerintah","Unit Transfusi Darah Milik PMI", "Unit Transfusi Darah Milik Kemenkes","Unit Transfusi Darah Milik Swasta")))),
            fluidRow(box(title = "Jumlah Unit Tranfusi Darah", plotlyOutput("map1"),width = 12)),
            fluidPage(
              sidebarPanel(width = 12,height = 12,style="text-align:center",
                           selectInput("x4", "Provinsi",
                                       choices = c('Aceh','Sumatera Utara','Sumatera Barat','Riau','Jambi','Sumatera Selatan','Bengkulu','Lampung','Kep. Bangka Belitung','Kepulauan Riau','DKI Jakarta','Jawa Barat','Jawa Tengah','DI Yogyakarta','Jawa Timur','Banten','Bali','Nusa Tenggara Barat','Nusa Tenggara Timur','Kalimantan Barat','Kalimantan Tengah','Kalimantan Selatan','Kalimantan Timur','Kalimantan Utara','Sulawesi Utara','Sulawesi Tengah','Sulawesi Selatan','Sulawesi Tenggara','Gorontalo','Sulawesi Barat','Maluku','Maluku Utara','Papua Barat','Papua')))),
            fluidRow(box(title = "Pie Chart", plotOutput("plot3")),
                     box(title = "Piramida Penduduk", plotOutput("plot1"))),
            fluidPage(
              sidebarPanel(width = 12,height = 5,style="text-align:center",
                           selectInput("x3", "Variabel",
                                       choices = c("Puskesmas Terakreditasi Dasar","Puskesmas Terakreditasi Madya", "Puskesmas Terakreditasi Utama","Puskesmas Terakreditasi Purna","Puskesmas Belum Terakreditasi","Rumah Sakit Pemerintah","Rumah Sakit Swasta","Klinik Pratama Pemerintah","Klinik Pratama TNI","Klinik Utama Pemerintah","Klinik Utama TNI")))),
            fluidRow(box(title = "Jumlah Pelayanan kesehatan", plotlyOutput("lol1"),width=12))
    ),
    tabItem(tabName = "penyakit",h3("Penyakit dan Kondisi Kesehatan", face = "bold"),
            fluidPage(
              sidebarPanel(width = 12,height = 5,style="text-align:center",
                           selectInput("xdf2", "Jenis Penyakit",
                                       choices = c('DBD','Malaria','filariasis','DM','TBC','HIV','AIDS','Kusta','Difteri','Purtosis','Neonatorum',
                                                   'Campak','Korban meninggal akibat bencana alam','Korban luka berat akibat bencana alam','Korban hilang akibat bencana alam',
                                                   'Korban luka ringan akibat bencana alam')))
            ),
            fluidRow(box(title = "Jumlah Tiap Jenis Penyakit di Indonesia ", plotlyOutput("map2"), width = 12)),
            fluidRow(box(title = "Histogram dan Densitas Tiap Jenis Penyakit", status = "primary", plotlyOutput("graph_3"), height = 450,width = 12),
                     box(title = "Pengelompokan Penyakit", status = "primary", plotlyOutput("plot5"), height = 450,width = 12)
            )),
    tabItem(tabName = "kebijakan",h3("Kebijakan Pengendalian Kesehatan", face = "bold"),
            sidebarPanel(width = 6,height = 5,style="text-align:center",
                         selectInput("var1_numerik", "Variabel 1",
                                     choices = c("% PVT","% PTM", "% POSBINDU","% PHBS","% Kabupaten Sehat","% Air Sehat", "% Jamban Sehat",
                                                 "% TU Sehat","% TPM Sehat"))),
            sidebarPanel(width = 6, height = 5, style="text-align:center",
                         selectInput("var2_numerik", "Variabel 2",
                                     choices = c("% PVT","% PTM", "% POSBINDU","% PHBS",
                                                 "% Kabupaten Sehat","% Air Sehat", "% Jamban Sehat",
                                                 "% TU Sehat","% TPM Sehat"))),
            fluidRow(box(title = "HUbungan Antar Program", status = "primary", plotlyOutput("graph31"), height = 450,width = 6),
                     box(title = "Nilai Korelasi", status = "primary", plotlyOutput("korelasi"), height = 450,width = 6),
                     box(title = "Densitas Data Antar Program", status = "primary", plotlyOutput("vio"), height = 450,width = 12)
            )
    ),
    tabItem(tabName = "kontak",h3("Profil Kementerian Kesehatan Indonesia"),
            tabsetPanel(
              tabPanel(h4("Visi dan Misi"),style="text-align:justify",h5("Visi", face="bold"),h5("Menciptakan manusia yang sehat, produktif, mandirim dan berkeadilan"),h5("Misi", face="bold"),
                       h5("Menurukan angka kematian ibu dan bayi"),
                       h5("Menurunkan angka stunting pada balita;"),
                       h5("Memperbaiki pengelolaan Jaminan Kesehatan Nasional"),
                       h5("meningkatkan kemandirian dan penggunaan produk farmasi dan alat kesehatan dalam negeri")),
              tabPanel(h4("Tujuan Strategis"),h5("Adapun yang menjadi tujuan strategis Kemenkes diantaranya:"),
                       h5("1. Peningkatan derajat kesehatan masyarakat melalui pendekatan siklus hidup"),
                       h5("2. Penguatan pelayanan kesehatan dasar dan rujukan"),
                       h5("3. Peningkatan pencegahan dan pengendalian penyakit dan pengelolaan kedaruratan kesehatan masyarakat"),
                       h5("4. Peningkatan sumber daya kesehatan")),
              tabPanel(h4("Kontak"),h5("KEMENTERIAN KESEHATAN RI", face="bold"),h5("Biro Komunikasi dan Pelayanan Masyarakat"),h5("Jl.H.R.Rasuna Said Blok X.5 Kav. 4-9, Kota Jakarta Selatan, Daerah Khusus Ibu Kota Jakarta 12950"),
                       h5("Telp. 021-5201590 (hunting)"),h5("Fax : (021) 52921669"),h5("Contact Center : 1500567 (Halo Kemkes)"),h5("E-mail: kontak@kemkes.go.id"))))
  ))

#server
server = function(input, output) {
  output$data_table=renderDataTable(mydata)
  
  output$summary_stat=renderPrint(descr(mydata,stats = "common"))
  
  output$summary_stat1=renderPrint(dfSummary(mydata))
  
  output$penduduk <- renderInfoBox({
    infoBox(
      "Jumlah Penduduk", paste0(sum(sum(df4$`Laki-Laki`,df4$Perempuan)),"jiwa"), icon = icon("user"),
      color = "green", fill = TRUE
    )
  })
  output$luas <- renderInfoBox({
    infoBox(
      "Luas Wilayah", paste0(sum(df1$luas),"km^2"), icon = icon("map-marker"),
      color = "green", fill = TRUE
    )
  })
  
  output$plot2=renderPlotly({
    if (input$'x2'=='Wanita Usia Subur') {
      bar1=ggplot(df1,show.legend = FALSE) +
        aes(
          x = provinsi,
          weight = wus
        ) +
        geom_bar(fill = "#023c40") +
        theme_lilik()+
        labs(
          x = "Provinsi",
          y = "Wanita Usia Subur (Dalam Juta Jiwa)"
        ) 
      ggplotly(bar1)
    } else if (input$'x2'=='Wanita Usia Subur Imunisasi') {
      bar1=ggplot(df1,show.legend = FALSE) +
        aes(
          x = provinsi,
          weight = wusi
        ) +
        geom_bar(fill = "#5bba6f") +
        theme_lilik()+
        labs(
          x = "Provinsi",
          y = "Wanita Usia Subur Imunisasi (Dalam Juta Jiwa)"
        ) 
      ggplotly(bar1)
    } else if (input$'x2'=='Ibu Bersalin') {
      bar1=ggplot(df1) +
        aes(
          x = provinsi,
          weight = ib
        ) +
        geom_bar(fill = "#28965a") +
        theme_lilik()+
        labs(
          x = "Provinsi",
          y = "Ibu Bersalin (Dalam Juta Jiwa)"
        ) 
      ggplotly(bar1)
    }else  {
      bar1=ggplot(df1) +
        aes(
          x = provinsi,
          weight = ih
        ) +
        geom_bar(fill = "#7bf1a8") +
        theme_lilik()+
        labs(
          x = "Provinsi",
          y = "Ibu Hamil (Dalam Juta Jiwa)"
        ) 
      ggplotly(bar1)
    }
  })
  output$plot1=renderPlot({
    laki=c(df4$`Laki-Laki`/10000000)
    perempuan=c(df4$Perempuan/10000000)
    umur=c(df4$Label)
    pir1 <- pyramid.plot(laki, perempuan,labels=umur, unit="Dalam Ratusan Jiwa",gap=1,top.labels=c("Laki-Laki","Umur","Perempuan"),
                         lxcol=c("#00afb9"),
                         rxcol=c("#f07167"))
    pir1
  })
  output$plot3=renderPlot({ 
    if (input$'x4'=='Aceh') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[1,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    } else if (input$'x4'=='Sumatera Utara') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[2,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    } else if (input$'x4'=='Sumatera Barat') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[3,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Riau') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[4,28:31])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Jambi') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[5,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Sumatera Selatan') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[6,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Bengkulu') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[7,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Lampung') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[8,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Kep. Bangka Belitung') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[9,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Kepulauan Riau') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[10,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='DKI Jakarta') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[11,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Jawa Barat') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[12,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Jawa Tengah') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[13,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='DI Yogyakarta') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[14,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Jawa Timur') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[15,28:31])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Banten') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[16,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Bali') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[17,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Nusa Tenggara Barat') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[18,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Nusa Tenggara Timur') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[19,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Kalimantan Barat') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[20,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Kalimantan Tengah') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[21,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Kalimantan Selatan') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[22,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Kalimantan Timur') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[23,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Kalimantan Utara') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[24,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Sulawesi Utara') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[25,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Sulawesi Tengah') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[26,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Sulawesi Selatan') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[27,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Sulawesi Tenggara') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[28,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Gorontalo') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[29,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Sulawesi Barat') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[30,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Maluku') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[31,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else if (input$'x4'=='Maluku Utara') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[32,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.26),col="black")
      pie
    }else if (input$'x4'=='Papua Barat') {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[33,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }else  {
      df=data.frame(
        dokter=c("Dokter Spesialis","Dokter Umum","Dokter Gigi Spesialis","Dokter Gigi"),
        jumlah=matrix(mydata[34,8:11])
      )
      pie=ggplot(df, aes(x = "", y = jumlah, fill = dokter)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+scale_fill_manual(values = c("#43aa8b","#90be6d","#f9c74f","#f8961e"))+theme_void()+
        geom_text_repel(aes(label = jumlah), position = position_stack(vjust = 0.5),col="black")
      pie
    }
  })
  
  output$lol1=renderPlotly({if (input$'x3'=="Puskesmas Terakreditasi Dasar") {
    lol1=ggplot(df1, aes(x=provinsi, y=pd)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=pd)) +
      geom_point( size=5, color="#78c6a3", fill=alpha("#99e2b4", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Puskesmas Terakreditasi Dasar"
      ) + theme_lilik()
    ggplotly(lol1)
  } else if (input$'x3'=="Puskesmas Terakreditasi Madya") {
    lol1=ggplot(df1, aes(x=provinsi, y=pm)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=pm)) +
      geom_point( size=5, color="#78c6a3", fill=alpha("#99e2b4", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Puskesmas Terakreditasi Madya"
      ) + theme_lilik()
    ggplotly(lol1)
  } else if (input$'x3'=="Puskesmas Terakreditasi Utama") {
    lol1=ggplot(df1, aes(x=provinsi, y=pu)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=pu)) +
      geom_point( size=5, color="#78c6a3", fill=alpha("#99e2b4", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Puskesmas Terakreditasi Utama"
      ) + theme_lilik()
    ggplotly(lol1)
  }else if (input$'x3'=="Puskesmas Terakreditasi Utama") {
    lol1=ggplot(df1, aes(x=provinsi, y=pu)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=pu)) +
      geom_point( size=5, color="#78c6a3", fill=alpha("#99e2b4", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Puskesmas Terakreditasi Utama"
      ) + theme_lilik()
    ggplotly(lol1)
  }else if (input$'x3'=="Puskesmas Terakreditasi Purna") {
    lol1=ggplot(df1, aes(x=provinsi, y=pp)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=pp)) +
      geom_point( size=5, color="#78c6a3", fill=alpha("#99e2b4", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Puskesmas Terakreditasi Purna"
      ) + theme_lilik()
    ggplotly(lol1)
  }else if (input$'x3'=="Puskesmas Belum Terakreditasi") {
    lol1=ggplot(df1, aes(x=provinsi, y=pb)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=pb)) +
      geom_point( size=5, color="#78c6a3", fill=alpha("#99e2b4", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Puskesmas Belum Terakreditasi"
      ) + theme_lilik()
    ggplotly(lol1)
  }else if (input$'x3'=="Rumah Sakit Pemerintah") {
    lol1=ggplot(df1, aes(x=provinsi, y=rsp)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=rsp)) +
      geom_point( size=5, color="#d0e040", fill=alpha("#c0e078", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Rumah Sakit Pemerintah"
      ) + theme_lilik()
    ggplotly(lol1)
  }else if (input$'x3'=="Rumah Sakit Swasta") {
    lol1=ggplot(df1, aes(x=provinsi, y=rss)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=rss)) +
      geom_point( size=5, color="#d0e040", fill=alpha("#c0e078", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Rumah Sakit Swasta"
      ) + theme_lilik()
    ggplotly(lol1)
  }else if (input$'x3'=="Klinik Pratama Pemerintah") {
    lol1=ggplot(df1, aes(x=provinsi, y=kpp)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=kpp)) +
      geom_point( size=5, color="#447604", fill=alpha("#eec019", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Klinik Pratama Pemerintah"
      ) + theme_lilik()
    ggplotly(lol1)
  }else if (input$'x3'=="Klinik Pratama TNI") {
    lol1=ggplot(df1, aes(x=provinsi, y=kpt)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=kpt)) +
      geom_point( size=5, color="#447604", fill=alpha("#eec019", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Klinik Pratama TNI"
      ) + theme_lilik()
    ggplotly(lol1)
  }else if (input$'x3'=="Klinik Utama Pemerintah") {
    lol1=ggplot(df1, aes(x=provinsi, y=kup)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=kup)) +
      geom_point( size=5, color="#447604", fill=alpha("#eec019", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Klinik Utama Pemerintah"
      ) + theme_lilik()
    ggplotly(lol1)
  }else  {
    lol1=ggplot(df1, aes(x=provinsi, y=kut)) +
      geom_segment( aes(x=provinsi, xend=provinsi, y=0, yend=kut)) +
      geom_point( size=5, color="#447604", fill=alpha("#eec019", 0.3), alpha=0.7, shape=21, stroke=2)+
      labs(
        x = "Provinsi",
        y = "Klinik Utama TNI"
      ) + theme_lilik()
    ggplotly(lol1)
  } 
  })
  
  output$map1=renderPlotly({
    if (input$'x1'=='Unit Transfusi Darah Milik Pemerintah') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df1,aes(x = longitude, y = latitude, size = utd1), 
                   color = "#319e72", alpha = 0.9, show.legend = F)+
        geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "#b7094c",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    } else if (input$'x1'=='Unit Transfusi Darah Milik PMI') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df1,aes(x = longitude, y = latitude, size = utd2), 
                   color = "#319e72", alpha = 0.9, show.legend = F)+
        geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "#b7094c",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    } else if (input$'x1'=='Unit Transfusi Darah Milik Kemenkes') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df1,aes(x = longitude, y = latitude, size = utd3), 
                   color = "#319e72", alpha = 0.9, show.legend = F)+
        geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "#b7094c",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df1,aes(x = longitude, y = latitude, size = utd4), 
                   color = "#319e72", alpha = 0.9, show.legend = F)+
        geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "#b7094c",show.legend=F, size=2)+
        theme_map()
    }
  })
  
  output$map2=renderPlotly({
    if (input$'xdf2'=='DBD') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = DBD), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    } else if (input$'xdf2'=='Malaria') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3)  
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = malaria), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    } else if (input$'xdf2'=='filariasis') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = filariasis), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='DM') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = DM), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='TBC') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "gray85", color = "gray80") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = TBC), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='HIV') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3)  
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = HIV), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='AIDS') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3)  
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = aids), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Kusta') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = kusta), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Difteri') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = difteri), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Purtosis') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3)  
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = purtosis), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Neonatorum') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3)   
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = neonatorum), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Campak') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = campak), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Korban meninggal akibat bencana alam') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = korbanmeninggal), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Korban luka berat akibat bencana alam') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3)  
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = lukaberat), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Korban luka ringan akibat bencana alam') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = lukaringan), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else  if (input$'xdf2'=='Korban hilang akibat bencana alam') {
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3)  
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = hilang), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }else{
      global <- map_data("world")
      gg1 <- ggplot() + 
        geom_polygon(data = global, aes(x=long, y = lat, group = group), 
                     fill = "#d9ed92", color = "#ccd5ae") + 
        coord_fixed(1.3) 
      
      Indo<-gg1 + xlim(94,142) + ylim(-11,7.5) +
        geom_point(data = df2,aes(x = longitude, y = latitude, size = pengungsi), 
                   color = "red", alpha = 0.5, show.legend = F)+geom_text(data = df1,aes(x = longitude, y = latitude,label= provinsi), color = "black",show.legend=F, size=2)+
        theme_map()
      
      ggplotly(Indo)
    }
  })
  
  output$graph_3=renderPlotly({
    if (input$'xdf2'=='DBD') {
      ggplotly(ggplot(df2, aes(x=DBD)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    } else if (input$'xdf2'=='Malaria') {
      ggplotly(ggplot(df2, aes(x=malaria)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    } else if (input$'xdf2'=='filariasis') {
      ggplotly(ggplot(df2, aes(x=filariasis)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='DM') {
      ggplotly(ggplot(df2, aes(x=DM)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='TBC') {
      ggplotly(ggplot(df2, aes(x=TBC)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='HIV') {
      ggplotly(ggplot(df2, aes(x=HIV)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='AIDS') {
      ggplotly(ggplot(df2, aes(x=aids)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Kusta') {
      ggplotly(ggplot(df2, aes(x=kusta)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Difteri') {
      ggplotly(ggplot(df2, aes(x=difteri)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Purtosis') {
      ggplotly(ggplot(df2, aes(x=purtosis)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Neonatorum') {
      ggplotly(ggplot(df2, aes(x=neonatorum)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Campak') {
      ggplotly(ggplot(df2, aes(x=campak)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Korban meninggal akibat bencana alam') {
      ggplotly(ggplot(df2, aes(x='korban meninggal')) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Korban luka berat akibat bencana alam') {
      ggplotly(ggplot(df2, aes(x='luka berat')) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Korban luka ringan akibat bencana alam') {
      ggplotly(ggplot(df2, aes(x='luka ringan')) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else  if (input$'xdf2'=='Korban hilang akibat bencana alam') {
      ggplotly(ggplot(df2, aes(x=hilang)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }else{
      global <- map_data("world")
      ggplotly(ggplot(df2, aes(x=pengungsi)) + 
                 geom_histogram(aes(y=..density..), fill="#30A278", bins=54L)+
                 geom_density(alpha=.5, fill="#b7e4c7", col="#30A278")+theme_lilik())
    }
  })
  
  output$plot5=renderPlotly({
    count <- ggplot(df2.count) +
      aes(x = kategori, fill = variable, weight = value) +
      geom_bar(position = "dodge") +
      scale_fill_manual(
        values = list(
          aids = "#2d6a4f",
          campak = "#d9ed92",
          DBD = "#0a9396",
          difteri = "#76c893",
          filariasis = "#94d2bd",
          HIV = "#1b4332",
          kusta = "#52b788",
          malaria = "#005f73",
          neonatorum = "#34a0a4",
          purtosis = "#168aad",
          TBC = "#b7e4c7"
        )
      ) +
      labs(x = "Jenis", y = "Jiwa") +
      theme_lilik()
    ggplotly(count)
    
  })
  
  output$graph31=renderPlotly({
    varx=df3[,input$var1_numerik]
    vary=df3[,input$var2_numerik]
    s <- ggplot(df3) +
      aes(
        x =varx,
        y =vary,
        colour = `Nama Provinsi`
      ) +
      geom_point(shape = "circle", size = 1.5) +
      scale_fill_brewer(palette="Spectral")+theme_minimal()+      
      geom_vline(xintercept=50, linetype="dashed", color = "red")+
      geom_hline(yintercept=50, linetype="dashed", color = "red")+theme(legend.position ="none") + 
      labs(x=input$var1_numerik, y=input$var2_numerik)+
      xlim(0, 100)+ylim(0, 100)
    ggplotly(s)
  })
  
  output$korelasi= renderPlotly({
    house.corr <-df3[,c(3:11)]
    corr <- round(cor(house.corr), 1)
    p.mat <- cor_pmat(house.corr)
    head(p.mat[,1:4])
    corplot <- ggcorrplot(corr,hc.order = T, outline.col = "white",lab = F,insig = "blank", 
                          type = "lower")
    ggplotly(corplot)
  })
  
  output$vio=renderPlotly({
    df3.vio <- df3[,-c(1:2,10:11)]
    df3.vio <- melt(df3)
    df3.vio <- df3.vio[-c(1:34, 341:408),]
    vio <- ggplot(df3.vio, aes(x = variable, y = value, fill = variable),trim = F) +
      geom_violin(alpha=0.5, width = 1) +
      scale_fill_manual(
        values = list(
          `% PVT` = "#F8766D",
          `% PTM` = "#D39200",
          `% POSBINDU` = "#93AA00",
          `% PHBS` = "#00BA38",
          `% Kabupaten Sehat` = "#00C19F",
          `% Air Sehat` = "#00B9E3",
          `% Jamban Sehat` = "#619CFF",
          `% TU Sehat` = "#DB72FB",
          `% TPM Sehat` = "#FF61C3"
        )
      ) +
      labs(
        x = "Program",
        y = "Presentase (%)") +
      theme_minimal() +
      theme(legend.position = "none") + geom_boxplot(fill='white', width=0.1) +
      stat_summary(geom = 'point', fun = mean,  size=1, fill='white')+theme_lilik()
    ggplotly(vio)
    
  })
  
}

ui<-dashboardPage(
  header=headerItem,
  sidebar = sidebarItem,
  body = bodyItem)
shinyApp(ui,server)
 


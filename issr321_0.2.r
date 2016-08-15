issr321 <- function(data_folder, h=50){

#Считываем исходные файлы
file_list<-list.files(data_folder, pattern='DATA_.+')


#выясняем число файлов = число повторностей
fn<-length(file_list)
for (i in 1:fn){
#генерим путь к файлу
	fpf<-paste0(data_folder,"/",file_list[i])
	if (i==1){
		list_of_source_data<-list(read.table(fpf, head=T, sep="\t", dec="."))
	}
	list_of_source_data[i]<-list(read.table(fpf, head=T, sep="\t", dec="."))
}



#v1<-read.table("p2v1.txt", head=T, sep="\t", dec=".")
#v2<-read.table("p2v2.txt", head=T, sep="\t", dec=".")
#v3<-read.table("p2v3.txt", head=T, sep="\t", dec=".")


#Отсортируем все матрицы по названиям образцов
for (i in 1:fn){
	list_of_source_data[[i]]<-list_of_source_data[[i]][with(list_of_source_data[[i]], order(Sample.File)), ]
}

#v1<-v1[with(v1, order(Sample.File)), ]
#v2<-v2[with(v2, order(Sample.File)), ]
#v3<-v3[with(v3, order(Sample.File)), ]

#tst<-data.frame(substr(v1[,1], 1, 5), substr(v2[,1], 1, 5), substr(v3[,1], 1, 5))

#сохраним названия образцов из первой матрицы для названий в матрице с итогами расчетов
sample_names<-substr(list_of_source_data[[1]][,1], 1, 5)

# определим число столбцов "Allele.n"
for (i in 1:fn){
	if (i ==1){
		num_a_col<-length(grep("Allele", names(list_of_source_data[[i]]), value=T))
	}
	num_a_col[i]<-length(grep("Allele", names(list_of_source_data[[i]]), value=T))
}
#num_a_col1 <- length(grep("Allele", names(v1), value=T))
#num_a_col2 <- length(grep("Allele", names(v2), value=T))
#num_a_col3 <- length(grep("Allele", names(v3), value=T))

#browser()

#определим координаты столбцов, содержащих Size (s_begin, s_end) и Height (h_begin, h_end)

for (i in 1:fn){
	if(i ==1){
		s_begin<-7+num_a_col[i]
	}
	s_begin[i]<-7+num_a_col[i]
	if(i ==1){
		s_end<-s_begin[i]+num_a_col[i]-1
	}
	s_end[i]<-s_begin[i]+num_a_col[i]-1
	
	if(i ==1){
		h_begin<-s_end[i]+1
	}
	h_begin[i]<-s_end[i]+1
	if(i ==1){
		h_end<-h_begin[i]+num_a_col[i]-1
	}
	h_end[i]<-h_begin[i]+num_a_col[i]-1
}
#browser()
#h_begin1<-s_end1+1
#h_end1<-h_begin1+num_a_col1-1

#s_begin2<-6+num_a_col2
#s_end2<-s_begin2+num_a_col2-1

#h_begin2<-s_end2+1
#h_end2<-h_begin2+num_a_col2-1

#s_begin3<-6+num_a_col3
#s_end3<-s_begin3+num_a_col3-1

#h_begin3<-s_end3+1
#h_end3<-h_begin3+num_a_col3-1

#избавимся от лишних столбцов в исходных матрицах


for (i in 1:fn){
	list_of_source_data[[i]]<-list_of_source_data[[i]][,s_begin[i]:h_end[i]]
}


#v1<-v1[,s_begin1:h_end1]
#v2<-v2[,s_begin2:h_end2]
#v3<-v3[,s_begin3:h_end3]

#ПЕРЕопределим координаты столбцов, содержащих Size и Height

for (i in 1:fn){
	
	s_begin[i]<-1
	s_end[i]<-s_begin[i]+num_a_col[i]-1

	h_begin[i]<-s_end[i]+1
	h_end[i]<-h_begin[i]+num_a_col[i]-1
}

#s_begin1<-1
#s_end1<-s_begin1+num_a_col1-1

#h_begin1<-s_end1+1
#h_end1<-h_begin1+num_a_col1-1

#s_begin2<-1
#s_end2<-s_begin2+num_a_col2-1

#h_begin2<-1
#h_end2<-h_begin2+num_a_col2-1

#s_begin3<-1
#s_end3<-s_begin3+num_a_col3-1

#h_begin3<-1
#h_end3<-h_begin3+num_a_col3-1

#заменим значения NA исходной матрице нулями для удобства обработки исходных матриц
for (i in 1:fn){
	list_of_source_data[[i]][is.na(list_of_source_data[[i]])] <-0
}
#browser()

#v1[is.na(v1)] <- 0
#v2[is.na(v2)] <- 0
#v3[is.na(v3)] <- 0


#---------------------------------------


#код для фильтрации значений размеров по высоте пика. Все размеры, высота пика которых окажется меньше заданной (по умолчанию - 50), будут обнулены.
#heights<- vector(length=0)

for (l in 1:fn){
	nrs<-nrow(list_of_source_data[[l]])
	ncs<-ncol(list_of_source_data[[l]])

	h_filter<-list_of_source_data[[l]][,h_begin[l]:h_end[l]]<h
	list_of_source_data[[l]][,s_begin[l]:s_end[l]][h_filter]<-0

}



#---------------------------------------

### определим размах значений размеров в матрицах
##for (i in 1:fn){
##	if(i==1){
##		min_s<-min(list_of_source_data[[i]][,1:s_end[i]])
##		max_s<-max(list_of_source_data[[i]][,1:s_end[i]])
##	}
##	min_s[i]<-min(list_of_source_data[[i]][,1:s_end[i]])
##	max_s[i]<-max(list_of_source_data[[i]][,1:s_end[i]])
##}

#Перепишем все ненулевые значения размеров из всех матриц в переменную szl
szl<-vector(length=0)
for (i in 1:fn){
	for (ri in 1:(nrow(list_of_source_data[[i]]))){
		for(ci in 1:s_end[i]){
			if (list_of_source_data[[i]][ri,ci]!=0){
				szl<-c(szl,list_of_source_data[[i]][ri,ci])				
			}
		}
	}
}


#browser()

#min_s_v1<-min(v1[, 1:s_end1])
#max_s_v1<-max(v1[, 1:s_end1])

#min_s_v2<-min(v2[, 1:s_end2])
#max_s_v2<-max(v2[, 1:s_end2])

#min_s_v3<-min(v3[, 1:s_end3])
#max_s_v3<-max(v3[, 1:s_end3])

###создадим вектор, в который загоним минимальные и максимальные значения размеров из каждого набора данных
##for (i in 1:fn){
##	if(i==1){
##		size_range<-c(min_s[i],max_s[i])
##	}
##	size_range<-c(size_range, min_s[i],max_s[i])

##}

###browser()
### округлим значения в векторе size_range
##size_range<-round(size_range)
##запомним значения ряда размеров фрагментов ДНК
##szs<-c(min(size_range):max(size_range))

szs<-unique(sort(round(szl)))

#Создание матрицы, которую надо заполнить значениями из отдельных повторностей
#Определим количество строк для этой матрицы и загоним в переменную nr

nr<-nrow(list_of_source_data[[1]])*fn

#определим количество столбцов в матрице
nc<-length(szs)

#создадим вектор, содержащий нули для заполнения столбцов
a<-rep(0,nr)

#создадим датафрейм, повторяющий вектор а по числу столбцов. Названия столбцов по размерам фрагментов

#dim(mat2fill)<-c(nr,nc)

mat2fill<-data.frame(length=0)
for (i in 1:(nc-1)){
	mat2fill<-cbind(mat2fill, data.frame(a))
}

colnames(mat2fill)<-szs


#наполним mat2fill данными из  исходных матриц. 
#Последовательно перебираем данные в исходных матрицах в каждой строке. Ищем значения размера, соответсвующие названию столбца в матрице mat2fill. Если есть такой размер - ставим 1. Если нет - ставим - 0.
#Строка 1 mat2fill должна соответствовать строке 1 v1, строка 2 mat2fill - строке 1 v2, строка 3 mat2fill - строке 1 из v3

#количество строк в исходной матрице
vnr<- nrow(list_of_source_data[[1]])

#----------------------------- 2016/08/06 22:15:16 продолжить тут -----------

#количество строк и столбцов в матрице mat2fill 

mnr<-nrow(mat2fill)
mnc<-ncol(mat2fill)

standard_sizes<-szs

sizes<-vector(length=0)
heights<-vector(length=0)

#цикл выполняем для каждой fn строки в матрице, подлежащей заполнению - mat2fill
#----------------временно

#fn<-1
#mnr<-15
#mnc<-10
#-----------------------

data_row_number<-1
for (l in 1:fn){
	for (rr in seq(l,mnr,fn)){
		if(data_row_number > 65){data_row_number<-1}
		#если число standard_sizes[i] есть в округленном векторе размеров фрагментов ДНК v1[rr,1:hstart] то пишем в mat2fill "1" в тот стобец, который соответствует найденному округленному размеру
		for (cc in 1:mnc){
			if (is.element(standard_sizes[cc], round(list_of_source_data[[l]][data_row_number,1:s_end[l]]))){
				mat2fill[rr,cc]<-1
			}
		}
		data_row_number<-data_row_number+1
	}
}


#----------------------------------------------------------------------------------------------

## Произведем схлопывание матрицы mat2fill по заданному критерию качества - qc
#при qc=1 если длина фрагмента встречается хотя бы 1 раз из 3-х она записывается в результирующую матрицу mat_result
#при qc=2 если длина фрагмента встречается 2 раза из 3-х она записывается в результирующую матрицу mat_result
#при qc=3 если длина фрагмента встречается 3 раз из 3-х она записывается в результирующую матрицу mat_result

#qc<-2



#Загоняем строку из условных девяток, чтобы задать размерность будущего датафрейма с результатами подсчетов встречаемости фрагмента каждого размера

mat_count<-t(data.frame(rep(9,mnc)))

#Создаем датафрейм mat_count, содержащий результаты подсчета встречаемости фрагмента каждого размера

for (rr in seq(1,mnr,fn)){
	
	new_row <- apply(mat2fill[(rr:(rr+(fn-1))),], 2, sum)
	mat_count<-rbind(mat_count, new_row)
	
}
#подчистим созданный датафрейм от лишнего ряда девяток и дадим для ориентировки названия строк по названиям образцов из матрицы v1
mat_count<-mat_count[2:nrow(mat_count),]

rownames(mat_count)<-sample_names

#Создаем таблицу с итогом расчетов в зависимости от значения критерия качества
# df[df=="" | df==12] <- NA
mat_result<-mat_count
for (l in 1:fn){
	if (l == 1){
		qc_mat_result<-list(mat_result)
	}
	qc_mat_result[l]<-list(mat_result)
}
for (l in 1:fn){
	qc_zero_filter<-qc_mat_result[[l]]<l
	qc_mat_result[[l]][qc_zero_filter]<-0
	qc_filter<-qc_mat_result[[l]]>=l
	qc_mat_result[[l]][qc_filter]<-1
	
	file_mat_count<-paste0(data_folder,"/qc_",l,"_mat_result.txt") 
	write.table(qc_mat_result[[l]],file_mat_count)
	#уничтожить стобцы, состоящие из одних нулей
	
	file_mat_result_no_zero_columns<-paste0(data_folder,"/qc_",l,"_mat_result_no_zero_columns.txt") 
	mat_result_no_zero_columns<-qc_mat_result[[l]][ , !apply(qc_mat_result[[l]]==0,2,all)]
	write.table(mat_result_no_zero_columns,file_mat_result_no_zero_columns)
}


#if (qc==2){
#	mat_result[mat_result==1]<-0
#	mat_result[mat_result>=2]<-1
#	file_mat_count<-paste0(data_folder,"/qc_2_mat_result.txt") 
#	write.table(mat_result,file_mat_count)
#	#уничтожить стобцы, состоящие из одних нулей
#	file_mat_result_no_zero_columns<-paste0(data_folder,"/qc_2_mat_result_no_zero_columns.txt") 
#	mat_result_no_zero_columns<-mat_result[ , !apply(mat_result==0,2,all)]
#	write.table(mat_result_no_zero_columns,file_mat_result_no_zero_columns)
#}

#if (qc==3){
#	mat_result[mat_result==1|mat_result==2]<-0
#	mat_result[mat_result>2]<-1
#	file_mat_count<-paste0(data_folder,"/qc_3_mat_result.txt") 
#	write.table(mat_result,file_mat_count)
#	#уничтожить стобцы, состоящие из одних нулей
#	file_mat_result_no_zero_columns<-paste0(data_folder,"/qc_3_mat_result_no_zero_columns.txt") 
#	mat_result_no_zero_columns<-mat_result[ , !apply(mat_result==0,2,all)]
#	write.table(mat_result_no_zero_columns,file_mat_result_no_zero_columns)
#}
##уничтожить стобцы, состоящие из одних нулей
#mat_result_no_zero_columns<-mat_result[ , !apply(mat_result==0,2,all)]
#write.table(mat_result_no_zero_columns,"mat_result_no_zero_columns.txt")




#file_mat_count<-paste0(data_folder, "/mat_count.txt")
#file_mat2fill<-paste0(data_folder, "/mat2fill.txt")

#write.table(mat_count, file_mat_count)
#write.table(mat2fill, file_mat2fill)
#browser()

return(print (paste0('Расчет окончен, начальник!')))
}


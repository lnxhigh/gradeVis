pre <- function (data) {
    data %>% 
        rename(
            year = '년도', semester = '학기',
            course_id = '학수번호', course_name = '과목명',
            major_la = '이수구분', la_area = '교양영역',
            course_type = '과목유형', 
            credit = '학점', score = '점수', grade = '등급', point = '평점', 
            ry = '재수강년도', rs = '재수강학기', rc = '재수강과목', 
            delete = '삭제구분'
        ) -> new
    return (new)
}

plotA <- function (data) {
    data %>%
        pre %>% 
        mutate(semester = case_when(semester == "여름계절" ~ 1, semester == "겨울계절" ~ 2, semester == 1 ~ 1, semester == 2 ~ 2)) %>% 
        mutate(point = ifelse(point == 0 & grade == "P", 4.5, point)) %>% 
        mutate(course_name = ifelse(!is.na(rc), rc, course_name)) %>% 
        mutate(ys = 10*year + semester, rys = 10*ry + as.numeric(substr(rs,1 , 1))) %>%
        mutate(ys = ifelse(!is.na(delete), rys, ys)) %>% 
        
        group_by(course_name) %>% 
        mutate(maxPoint = max(point)) %>% 
        slice(which.max(ys)) %>%
        filter(grade != "F") %>% 
        
        group_by(ys) %>%
        summarise(creditSum = sum(credit)) %>% 
        mutate(ys = factor(ys, labels = seq_len(n()))) %>% 
        
        ggplot(aes(x = ys, y = cumsum(creditSum))) +
        geom_col(width = 2/3) +
        geom_text(aes(label = cumsum(creditSum), vjust = -1)) +
        scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
        theme_bw() +
        theme(text = element_text(size = 16, family = "serif")) +
        xlab("Semester") + ylab("Total Credit") + ggtitle("Cumulative Credit") -> p
    
    return (p)
}

####################################################################################################################################

plotB <- function (data, type = c("r", "n", "b"), 
    rtitle = "GPA change reflecting repeated course", ntitle = "GPA change not reflecting repeated course") {
    data %>%
        pre %>% 
        mutate(semester = case_when(semester == "여름계절" ~ 1, semester == "겨울계절" ~ 2, semester == 1 ~ 1, semester == 2 ~ 2)) %>% 
        mutate(point = ifelse(point == 0 & grade == "P", 4.5, point)) %>% 
        mutate(course_name = ifelse(!is.na(rc), rc, course_name)) %>% 
        mutate(ys = 10*year + semester, rys = 10*ry + as.numeric(substr(rs,1 , 1))) %>%
        mutate(ys = ifelse(!is.na(delete), rys, ys)) %>% 
        
        group_by(course_name) %>% 
        mutate(maxPoint = max(point)) %>% 
        slice(which.max(ys)) %>%
        
        group_by(ys) %>% 
        summarise(pointavg = weighted.mean(maxPoint, credit), creditSum = sum(credit)) -> df1
    
    data %>%
        pre %>% 
        mutate(semester = case_when(semester == "여름계절" ~ 1, semester == "겨울계절" ~ 2, semester == 1 ~ 1, semester == 2 ~ 2)) %>% 
        mutate(point = ifelse(point == 0 & grade == "P", 4.5, point)) %>% 
        mutate(ys = 10*year + semester) %>% 
        
        group_by(ys) %>% 
        summarise(pointavg = weighted.mean(point, credit), creditSum = sum(credit)) -> df2
    
    
    if (type == "r") {
        df1 %>% 
            ggplot(aes(x = seq_along(ys), y = pointavg)) +
            geom_point() + geom_line() + geom_hline(aes(yintercept = weighted.mean(pointavg, creditSum))) +
            geom_text(aes(label = round(pointavg, 2), vjust = -1)) +
            theme_bw() +
            theme(text = element_text(size = 16, family = "serif")) +
            xlab("Semester") + ylab("Grade Point Average(GPA)") + ggtitle(rtitle) -> p1
        
        return (p1)
    }
    
    if (type == "n") {
        df2 %>% 
            ggplot(aes(x = seq_along(ys), y = pointavg)) +
            geom_point() + geom_line() + geom_hline(aes(yintercept = weighted.mean(pointavg, creditSum))) +
            geom_text(aes(label = round(pointavg, 2), vjust = -1)) +
            theme_bw() +
            theme(text = element_text(size = 16, family = "serif")) +
            xlab("Semester") + ylab("Grade Point Average(GPA)") + ggtitle(ntitle) -> p2
        
        return (p2)
    }
    
    if (type == "b") {
        ggplot(df1, aes(x = seq_along(ys), y = pointavg)) +
            geom_point(col = "royalblue") + geom_line(col = "royalblue") + 
            geom_hline(aes(yintercept = weighted.mean(pointavg, creditSum)), col = "royalblue") +
            geom_point(data = df2, mapping = aes(x = seq_along(ys), y = pointavg), col = "orangered") +
            geom_line(data = df2, mapping = aes(x = seq_along(ys), y = pointavg), col = "orangered") + 
            geom_hline(data = df2, mapping = aes(yintercept = weighted.mean(pointavg, creditSum)), col = "orangered") +
            theme_bw() +
            theme(text = element_text(size = 16, family = "serif")) +
            xlab("Semester") + ylab("Grade Point Average(GPA)") + 
            ggtitle("Graph showing both",
                subtitle = "Blue - With Repeated Course | Red - Without Repeated Course") -> p3
        
        return (p3)
    }
}

####################################################################################################################################

plotC <- function (data, type = c("ABCDF", "PF")) {
    
    if (type == "ABCDF") {
        data %>%
            pre %>% 
            mutate(semester = case_when(semester == "여름계절" ~ 1, semester == "겨울계절" ~ 2, 
                semester == 1 ~ 1, semester == 2 ~ 2)) %>% 
            mutate(point = ifelse(point == 0 & grade == "P", NA, point)) %>% 
            filter(!is.na(point)) %>% 
            
            mutate(course_name = ifelse(!is.na(rc), rc, course_name)) %>% 
            mutate(ys = 10*year + semester, 
                rys = 10*ry + as.numeric(substr(rs,1 , 1))) %>%
            mutate(ys = ifelse(!is.na(delete), rys, ys)) %>% 
            
            group_by(course_name) %>% 
            mutate(maxPoint = factor(max(point),
                levels = c(4.5, 4, 3.5, 3, 2.5, 2, 1.5, 1, 0), 
                labels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D", "F"))) %>% 
            slice(which.max(ys)) %>%
            
            ggplot(aes(x = maxPoint, fill = maxPoint)) +
            geom_bar(width = 2/3) + 
            geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
            scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
            theme_bw() +
            theme(text = element_text(size = 16, family = "serif")) +
            theme(legend.position = "none") +
            xlab("Grade") + ylab("Count") + ggtitle("ABCDF Grade Distribution") -> p_ABCDF
        
        return (p_ABCDF)
    }
    
    if (type == "PF") {
        data %>% 
            pre %>% 
            filter(grade %in% c("P", "F")) %>% 
            mutate(grade = factor(grade, level = c("P", "F"))) %>% 
            ggplot(aes(x = grade, fill = grade)) +
            geom_bar() +
            geom_text(stat = 'count', aes(label = ..count..), 
                vjust = -1, size = 5, family = "serif") +
            scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
            scale_fill_manual(values = c("royalblue", "orangered")) +
            theme_bw() +
            theme(legend.position = "top") +
            theme(text = element_text(size = 16, family = "serif")) +
            theme(legend.position = "top", legend.justification = 'left', legend.direction = 'horizontal') +
            guides(fill = guide_legend(title = "")) +
            xlab("Grade") + ylab("Count") + ggtitle("PF grade distribution") -> p_PF
        
        return (p_PF)
    }
}

####################################################################################################################################

plotD <- function (data, type = c("ts", "bp")) {
    data %>% filter(is.na(`교양영역`)) -> major
    data %>% filter(!is.na(`교양영역`)) -> general
    
    if (type == "ts") {
        plotB(major, "r", rtitle = "GPA change in Major") -> p1
        plotB(general, "r", rtitle = "GPA change in Liberal Arts") -> p2
        
        return (gridExtra::grid.arrange(p1, p2, nrow = 1))
    }
    
    if (type == "bp") {
        data %>% 
            pre %>% 
            mutate(ml = factor(ifelse(is.na(la_area), "Major", "Liberal Arts"), levels = c("Major", "Liberal Arts"))) %>% 
            mutate(point = ifelse(point == 0 & grade == "P", 4.5, point)) %>%
            
            group_by(ml) %>% 
            summarise(gpa = mean(point)) %>% 
            
            ggplot(aes(x = ml, y = gpa, fill = ml)) +
            geom_col() +
            geom_text(aes(label = round(gpa, 2), vjust = -1), family = "serif") +
            theme(text = element_text(size = 16, family = "serif")) +
            theme_bw() +
            xlab("Type") + ylab("Grade Point Average(GPA)") + ggtitle("Major vs. Liberal Arts") +
            theme(legend.position = "top", legend.justification = 'left', legend.direction = 'horizontal') +
            scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
            scale_fill_manual(values = c("orangered", "royalblue")) +
            guides(fill = guide_legend(title = "")) +
            theme(text = element_text(size = 16, family = "serif")) -> p3
        
        return (p3)
    }
}

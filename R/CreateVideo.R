 
# # Create some PNG images
# png("~/test/input%03d.png", width = 1280, height = 720, res = 108)
# for(i in 1:10){
#   print(ggplot2::qplot(rnorm(100)))
# }
# dev.off()
# png_files <- sprintf("~/test/input%03d.png", 1:10)
# av::av_encode_video(png_files, '~/output.mp4', framerate = 1)
# utils::browseURL('~/output.mp4')
# 
# # make direct
# video_file <- file.path(tempdir(), 'output.mp4')
# av::av_capture_graphics(makeplot(), video_file, 1280, 720, res = 144)
# utils::browseURL(video_file)
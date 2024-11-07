x = seq(0, 3, by = 0.01)
f = sin(x)
g = sin(x) + 0.05 * cos(x) 
df = as.data.frame(x = x, f = f, g = g)

ggplot(data = df, mapping = aes(x = x)) + geom_line(aes(y = g, colour = 'red'), linetype = 4) + geom_line(aes(y = f, colour = 'black'))+ xlim(0, 3) + scale_color_identity(" ", labels = c(expression(paste(f)), expression(paste("  ", f, '+', Delta, f))), guide = "legend") + theme_bw() + geom_ribbon(aes(ymin=f,ymax=g), fill="red", alpha="0.1") + geom_ribbon(aes(ymin = 0,ymax = f), fill="grey", alpha="0.1") + ylab("")
)

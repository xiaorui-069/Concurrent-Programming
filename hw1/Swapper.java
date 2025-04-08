public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        // TODO: Implement me!
        String str = content.substring(interval.getX(), interval.getY() + 1);
        for(char c : str.toCharArray()){
            buffer[offset++] = c;
        }
    }
}
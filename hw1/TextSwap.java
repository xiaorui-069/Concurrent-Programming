import java.io.*;
import java.util.*;

public class TextSwap {

    private static String readFile(String filename, int chunkSize) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        if (file.length() % chunkSize!=0){ 
            throw new Exception("File size not multiple of chunk size"); 
        }
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null){
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        // TODO: Implement me!
        Interval[] intervals = new Interval[numChunks];
        int start = 0, i = 0;
        while (i < numChunks) {
            intervals[i] = new Interval(start, start + chunkSize - 1);
            start += chunkSize;
            i++;
        }
        return intervals;
    }

    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        return labels;
    }

    private static char[] runSwapper(String content, int chunkSize, int numChunks) {
    List<Character> labels = getLabels(numChunks);
    Interval[] intervals = getIntervals(numChunks, chunkSize);
    char[] buffer = new char[content.length()];
    List<Thread> threads = new ArrayList<>();

    for (int i = 0; i < labels.size(); i++) {
        Swapper swapper = new Swapper(intervals[labels.get(i) - 'a'], content, buffer, i * chunkSize);
        Thread thread = new Thread(swapper);
        threads.add(thread);
        thread.start();
    }

    for (Thread thread : threads) {
        try {
            thread.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            e.printStackTrace();
        }
    }

    return buffer;
}

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        if(numChunks > 26) throw new Exception("Chunk size too small");
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

     public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);
        try {
            contents = readFile(args[1],chunkSize);
            writeToFile(contents, chunkSize, contents.length() / chunkSize);
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
    }
}

// NAME: XIAORUI GUO
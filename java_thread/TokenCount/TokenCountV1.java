import java.util.HashMap;


public class TokenCountV1 extends TokenCountBase {

	public class Producer implements Runnable {
		Iterable<Page> allPages;

		public Producer(Iterable<Page> pages) {
			this.allPages = pages;
		}

		public void run() {
			for (Page pg : allPages) {
				putPage(pg);
			}
			producingFinish = true;
		}
	}

	public class Consumer implements Runnable {

		public void run() {
			Page pg = getPage();
			while (pg != null && !producingFinish) {
				Iterable<String> allTokens = new Words(pg.getText());
				for (String s : allTokens) {
					countToken(s);
				}
				pg = getPage();
			}
		}
	}

	public TokenCountV1(int num, String file) {
		super(num, file);
		this.tokenFreq = new HashMap<String, Integer>();
	}
	
	public static long test(int numPages, String pageFile) {
		TokenCountV1 tokenCount = new TokenCountV1(numPages, pageFile);

		Thread producerThread = new Thread(tokenCount.new Producer(
				tokenCount.allPages));
		Thread consumerThread = new Thread(tokenCount.new Consumer());

		final long before = System.nanoTime();
		producerThread.start();
		consumerThread.start();

		try {
			producerThread.join();
			consumerThread.join();
		} catch (Exception e) {
			e.printStackTrace();
		}

		final long after = System.nanoTime();
		long costSec = (after - before) / 1000000;
		System.out.println("Time to process " + numPages + " pages = "
				+ costSec + " milliseconds");

		tokenCount.wordStatistics();
		return costSec;
	}
	
	public static void main(String[] args) {
		if (args.length != 2) {
		    System.out.println("usage: java TokenCount number-of-pages XML-file");
		    System.exit(0);
		}
		Integer numPages = Integer.parseInt(args[0]);
		String pageFile = args[1];
		
		test(numPages, pageFile);
	}

}

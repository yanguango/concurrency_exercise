import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class TokenCountV2 extends TokenCountBase {

	protected void countToken(String tok) {
		synchronized (tokenFreq) {
			Integer currentCount = tokenFreq.get(tok);
			if (currentCount == null)
				tokenFreq.put(tok, 1);
			else
				tokenFreq.put(tok, currentCount + 1);
		}
	}
	
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

	public TokenCountV2(int num, String file) {
		super(num, file);
		this.tokenFreq = new HashMap<String, Integer>();
	}

	public static long test(int numPages, String pageFile) {
		int numProcs = Runtime.getRuntime().availableProcessors() - 1;

		ExecutorService pool = Executors.newCachedThreadPool();

		TokenCountV2 tokenCount = new TokenCountV2(numPages, pageFile);

		Thread producerThread = new Thread(tokenCount.new Producer(
				tokenCount.allPages));

		List<Future<?>> futures = new ArrayList<Future<?>>();

		final long before = System.nanoTime();
		producerThread.start();
		for (int i = 0; i < numProcs - 1; i++) {
			futures.add(pool.submit(tokenCount.new Consumer()));
		}

		try {
			producerThread.join();
			for (Future<?> future : futures) {
				future.get();
			}
			pool.shutdown();
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
			System.out
					.println("usage: java TokenCount number-of-pages XML-file");
			System.exit(0);
		}
		
		Integer numPages = Integer.parseInt(args[0]);
		String pageFile = args[1];
		
		test(numPages, pageFile);

	}

}

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class TokenCountV4 extends TokenCountBase {

	public void countToken(String tok, ConcurrentHashMap<String, Integer> map) {
		Integer currentCount = map.get(tok);
		if (currentCount == null)
			map.put(tok, 1);
		else
			map.put(tok, currentCount + 1);
	}

	public void mergeMap(ConcurrentHashMap<String, Integer> map) {
		Iterator<Map.Entry<String, Integer>> it = map.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry<String, Integer> pairs = (Map.Entry<String, Integer>) it
					.next();
			String key = pairs.getKey();
			Integer val = pairs.getValue();
			if (tokenFreq.containsKey(key)) {
				Integer oldVal = tokenFreq.get(key);
				Integer newVal = Integer.valueOf(oldVal.intValue()
						+ val.intValue());
				tokenFreq.put(key, newVal);
			} else {
				tokenFreq.put(key, val);
			}
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
		ConcurrentHashMap<String, Integer> map;

		public Consumer() {
			map = new ConcurrentHashMap<String, Integer>();
		}

		public void run() {
			Page pg = getPage();
			while (pg != null && !producingFinish) {
				Iterable<String> allTokens = new Words(pg.getText());

				for (String s : allTokens) {
					countToken(s, map);
				}
				pg = getPage();
			}

			mergeMap(map);
		}
	}

	public TokenCountV4(int num, String file) {
		super(num, file);
		this.tokenFreq = new ConcurrentHashMap<String, Integer>();
	}

	public static long test(int numPages, String pageFile) {
		int numProcs = Runtime.getRuntime().availableProcessors() - 1;

		ExecutorService pool = Executors.newCachedThreadPool();

		TokenCountV4 tokenCount = new TokenCountV4(numPages, pageFile);

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

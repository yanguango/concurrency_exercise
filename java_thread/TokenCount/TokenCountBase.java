import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;

public class TokenCountBase {
	protected final int QUEUE_SIZE = 10;
	protected volatile ArrayBlockingQueue<Page> queue;
	protected Iterable<Page> allPages;
	protected boolean producingFinish;
	protected AbstractMap<String, Integer> tokenFreq;

	protected void wordStatistics() {
		Set<Entry<String, Integer>> entries = tokenFreq.entrySet();
		ArrayList<Entry<String, Integer>> list = new ArrayList<Entry<String, Integer>>(
				entries);
		Collections.sort(list, new Comparator<Map.Entry<String, Integer>>() {
			public int compare(Map.Entry<String, Integer> obj1,
					Map.Entry<String, Integer> obj2) {
				return (obj2.getValue()).compareTo(obj1.getValue());
			}
		});

		for (int i = 0; i < 30; i++) {
			System.out.println(list.get(i).getKey() + " appears "
					+ list.get(i).getValue() + " times");
		}
	}

	protected void countToken(String tok) {
		Integer currentCount = tokenFreq.get(tok);
		if (currentCount == null)
			tokenFreq.put(tok, 1);
		else
			tokenFreq.put(tok, currentCount + 1);
	}

	public TokenCountBase(int num, String file) {
		this.queue = new ArrayBlockingQueue<Page>(QUEUE_SIZE);
		this.allPages = new Pages(num, file);
		this.producingFinish = false;
	}
	
	public synchronized void putPage(Page pg) {
		while (queue.size() == QUEUE_SIZE) {
			try {
				wait();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		queue.add(pg);
		notify();
	}

	public synchronized Page getPage() {
		Page pg = null;

		while (queue.isEmpty()) {
			try {
				wait();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		pg = queue.poll();
		notify();
		return pg;
	}
}

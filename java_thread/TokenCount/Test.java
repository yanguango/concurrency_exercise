
public class Test {

	public static void main(String[] args) {
		if (args.length != 2) {
		    System.out.println("usage: java TokenCount number-of-pages XML-file");
		    System.exit(0);
		}
		Integer numPages = Integer.parseInt(args[0]);
		String pageFile = args[1];
		
		long cost1 = TokenCountV1.test(numPages, pageFile);
		long cost2 = TokenCountV2.test(numPages, pageFile);
		long cost3 = TokenCountV3.test(numPages, pageFile);
		long cost4 = TokenCountV4.test(numPages, pageFile);
		
		System.out.println("V1: " + cost1 + " milliseconds");
		System.out.println("V2: " + cost2 + " milliseconds");
		System.out.println("V3: " + cost3 + " milliseconds");
		System.out.println("V4: " + cost4 + " milliseconds");
	}

}

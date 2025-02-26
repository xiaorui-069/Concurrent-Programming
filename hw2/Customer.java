import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CountDownLatch;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingList;
    private int shopTime;
    private int checkoutTime;
    private CountDownLatch doneSignal;
    
    /**
     * Initialize a customer object and randomize its shopping list
     */
    public Customer(Bakery bakery, CountDownLatch l) {
        // TODO
        this.bakery = bakery;
        this.rnd = new Random();
        this.shoppingList = new ArrayList<>();
        this.shopTime = 1000 + rnd.nextInt(1000);
        this.checkoutTime = 1000 + rnd.nextInt(1000);
        this.doneSignal = l;
        fillShoppingList();
        System.out.println(toString());
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO
        try {

            bakery.shelves.acquire();
            System.out.println("Customer " + hashCode() + " starts shopping.");
            Thread.sleep(shopTime);
            for (BreadType bt : shoppingList) {
                System.out.println("Customer " + hashCode() + " takes " + bt + " from stock.");
                bakery.takeBread(bt);
            }
            System.out.println("Customer " + hashCode() + " finishes shopping.");
            bakery.shelves.release();

            bakery.cashiers.acquire();
            System.out.println("Customer " + hashCode() + " is checking out.");
            Thread.sleep(checkoutTime);
            bakery.mutex_sales.acquire();
            bakery.addSales(getItemsValue());
            bakery.mutex_sales.release();
            System.out.println("Customer " + hashCode() + " paid and left.");
            bakery.cashiers.release();

        } catch (InterruptedException ie) {
            ie.printStackTrace();
        }
        doneSignal.countDown();
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingList=" + Arrays.toString(shoppingList.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping list
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingList.size() >= 3) {
            return false;
        }
        shoppingList.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping list with 1 to 3 random breads
     */
    private void fillShoppingList() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping list
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingList) {
            value += bread.getPrice();
        }
        return value;
    }
}

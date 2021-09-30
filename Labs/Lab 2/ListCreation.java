import java.util.*;


public class ListCreation {
    public static void main(String[] args) {
        //Read in user input with scanner and call function to generate the list
        Scanner input = new Scanner(System.in);
        System.out.println("Enter the amount of lists you wish for");
        int numLists = input.nextInt();

        System.out.println("Enter the size of each list");
        int size = input.nextInt();

        input.close();

 
        generateList(numLists, size);

    }
    
    public static void generateList(int numLists, int size) {
        int[][] list = new int[numLists + 1][size + 1];
        //Outer loop traverses through each row and keeps track of the number of lists
        for (int i = 1; i < numLists + 1; i++) {
            //Inner loop traverses through each column of the two dimensional array
            for (int j = 1; j < size + 1; j++) {
                // j * numLists in order to have each element follow the same sequence
                // subtract i and add one for the columns so the bottom row starts at 1 not 0
                list[i][j] = j * numLists - i + 1;
                System.out.print(list[i][j]  + "\t");
            }
            System.out.println();
        }
    }
}

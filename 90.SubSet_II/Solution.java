import java.util.Set;
import java.util.HashSet;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

class Solution {
    private static List<List<Integer>> res;
    private static int[] nums;
    
    public static List<List<Integer>> subsetsWithDup(int[] nums) {
        Arrays.sort(nums);
        this.res = new ArrayList<>();
        this.nums = nums;
        List<Integer> init = new ArrayList<>();
        helper(0, init);
        Set<List<Integer>> tmp = new HashSet<>(res);
        return new ArrayList<List<Integer>>(tmp);
    }
    
    private static void helper(int start, List<Integer> list) {
        if (start == nums.length) {
            res.add(list);
        }
        if (start < nums.length) {
            List<Integer> tmp = new ArrayList<>(list);
            helper(start + 1, tmp);
            tmp = new ArrayList<>(list);
            tmp.add(nums[start]);
            helper(start + 1, tmp);
        }
    }
    
    
}
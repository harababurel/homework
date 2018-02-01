package ml.sergiu.mobile_exam;

import android.arch.persistence.room.Dao;
import android.arch.persistence.room.Delete;
import android.arch.persistence.room.Insert;
import android.arch.persistence.room.Query;

import java.util.List;

@Dao
public interface CarDao {

    @Query("SELECT * FROM car")
    List<Car> getAll();

    @Query("SELECT * FROM car where name LIKE :name")
    Car findByName(String name);

    @Query("SELECT COUNT(*) from car")
    int countCars();

    @Insert
    void insertAll(List<Car> cars);

    @Delete
    void delete(Car car);


    @Query("DELETE FROM car")
    void deleteAll();
}

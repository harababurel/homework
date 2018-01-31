package ml.sergiu.mobile_exam;

import android.util.Log;

import com.loopj.android.http.AsyncHttpClient;
import com.loopj.android.http.AsyncHttpResponseHandler;
import com.loopj.android.http.JsonHttpResponseHandler;
import com.loopj.android.http.RequestParams;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import cz.msebera.android.httpclient.Header;


public class CarClient {
    private URI base_url;
    private AsyncHttpClient client;

    public CarClient(URI base_url) {
        this.base_url = base_url;
        client = new AsyncHttpClient();

    }

    public void get(String path, RequestParams params, AsyncHttpResponseHandler responseHandler) {
        Log.d("BOBS", "GET-ing " + resolvePath(path));
        client.get(resolvePath(path), params, responseHandler);
    }

    public void post(String path, RequestParams params, AsyncHttpResponseHandler responseHandler) {
        Log.d("BOBS", "POST-ing " + resolvePath(path));
        client.post(resolvePath(path), params, responseHandler);
    }


    public void delete(String path, RequestParams params, AsyncHttpResponseHandler
            responseHandler) {
        Log.d("BOBS", "DELETE-ing " + resolvePath(path));
        client.delete(resolvePath(path), params, responseHandler);
    }

    private String resolvePath(String path) {
        return base_url.resolve(path).toString();
    }

    public void getCars(final GetCarsListener listener) {
        this.get("/cars", null, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONArray response) {
                Log.i("BOBS", response + "");

                List<Car> cars = new ArrayList<>();
                for (int i = 0; i < response.length(); i++) {
                    try {
                        cars.add(new Car(response.getJSONObject(i)));
                    } catch (JSONException e) {
                        Log.e("CARS", "Bad car json: " + e);
                    }
                }

                listener.processGet(cars);
            }
        });
    }

    public void addCar(final AddCarListener listener, final Car car) {
        RequestParams params = new RequestParams();
        params.put("name", car.name);
        params.put("quantity", car.quantity);
        params.put("type", car.type);
        params.put("status", car.status);

        this.post("/addCar", params, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONObject response) {
                Log.i("BOBS", response + "");

                try {
                    listener.processAdd(new Car(response));
                } catch (JSONException e) {
                    Log.e("CARS", "Bad car json: " + e);
                }
            }
        });
    }

    public void removeCar(final RemoveCarListener listener, final Car car) {
        RequestParams params = new RequestParams();
        params.put("id", car.id);

        this.post("/removeCar", params, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONObject response) {
                listener.processRemove(car);
                Log.i("BOBS", "Car successfully deleted");
            }
        });
    }
}

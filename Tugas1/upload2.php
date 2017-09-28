
<!-- +++++ Contact Section +++++ -->
	
	<div class="container pt">
		<div class="row mt">
			<div class="col-lg-6 col-lg-offset-3 centered">
				<h3>UPLOAD UNDANG-UNDANG</h3>
				<hr>
				<p>Silahkan upload file .pdf untuk melakukan proses tokenisasi,stopword removal, dan stemming.</p>
			</div>
		</div>
		<div class="row mt">	
			<div class="col-lg-8 col-lg-offset-2">
				<form enctype="multipart/form-data" method="POST" action="?menu=hasil_upload">
				  <div class="form-group">
				  	<input type="file" name="fupload" class="form-control">
				    <br>
				  </div>
				  
				  <textarea class="form-control" rows="6" name="deskripsi" placeholder="Deskripsi"></textarea>
				    <br>
				  <button type="submit" class="btn btn-success">UPLOAD</button>
				</form>    			
			</div>
		</div><!-- /row -->
	</div><!-- /container -->
	
	
	<!-- +++++ Footer Section +++++ -->
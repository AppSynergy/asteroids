var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber');
var uglify = require('gulp-uglify');
var watch = require('gulp-watch')

gulp.task('elm-init', elm.init);

gulp.task('watch', function() {
  gulp.watch('src/*', ['elm']);
});

gulp.task('elm', ['elm-init'], function(){
  return gulp.src('src/*.elm')
    .pipe(plumber())
    .pipe(elm())
    .pipe(uglify())
    .on('error', swallowError)
    .pipe(gulp.dest('dist/'));
});

gulp.task('index', function(){
  return gulp.src('src/index.html')
    .pipe(gulp.dest('dist/'));
});

gulp.task('default', ['elm', 'index']);

swallowError = function(error){
  console.log(error.toString());
  this.emit('end');
}
